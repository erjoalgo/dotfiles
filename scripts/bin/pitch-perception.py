#!/usr/bin/python

"""An interactive game to test the user's pitch perception abilities.


Inspired by "Measure your pitch perception abilities"
from http://jakemandell.com/adaptivepitch.
"""

import contextlib
import curses
import random
import subprocess
import time



class PitchPerceptionGameConfig(object):
    """Config for a PitchPerceptionGame."""
    def __init__(self,
                 initial_delta=220,
                 ref_tone=440,
                 max_incorrect_attempts=2,
                 min_correct_attempts=3):
        self.initial_delta = initial_delta
        self.ref_tone = ref_tone
        self.max_incorrect_attempts = max_incorrect_attempts
        self.min_correct_attempts = min_correct_attempts


class UserQuitError(Exception):
    """Propagates a user quit up the stack."""
    pass

class PitchPerceptionChallenge(object):
    """An interactive game to test the user's pitch perception abilities."""
    def __init__(self, game_config):
        self.game_config = game_config
        self.win = None

    @staticmethod
    def beep(freq):
        """Beep at the given frequency in Hz."""
        cmd = ["beep", "-f", str(freq)]
        subprocess.call(cmd)

    def curses_print(self, message):
        self.win.clear()
        self.win.addstr(0, 0, message)
        self.win.refresh()

    def curses_getch(self):
        ch = self.win.getch()
        if ch == 27:
            ch = self.win.getch()
            assert ch == 91
            ch = self.win.getch()
        return ch

    def _play_tone_challenge(self, first, second):
        assert first != second
        self.beep(first)
        self.beep(second)
        prompt = ("Is the second pitch higher or lower "
                  "(Up: higher, Down: lower, r: repeat, q: quit)? ")
        UP, DOWN, REPEAT, QUIT = 65, 66, 114, 113
        while True:
            self.curses_print(prompt)
            ch = self.curses_getch()
            if ch == REPEAT:
                self.beep(first)
                self.beep(second)
            elif ch in (UP, DOWN):
                return (second > first) == (ch == UP)
            elif ch == QUIT:
                raise UserQuitError()
            else:
                self.curses_print("unknown key: {}".format(ch))
                time.sleep(2)

    @staticmethod
    def flip_coin():
        return random.randint(0, 1)

    def _challenge_at_delta(self, delta):
        """Returns true iff the user can distinguish pitch up to delta Hz."""
        assert delta > 0
        correct, incorrect = 0, 0

        while True:
            first = self.game_config.ref_tone
            second = first + delta * (1 if self.flip_coin() else -1)
            if self.flip_coin():
                first, second = second, first
            is_correct = self._play_tone_challenge(first, second)
            if is_correct:
                correct += 1
                if correct >= self.game_config.min_correct_attempts:
                    return True
            else:
                incorrect += 1
                self.curses_print("incorrect: {} => {}".format(first, second))
                time.sleep(1)
                if incorrect >= self.game_config.max_incorrect_attempts:
                    return False

    @staticmethod
    @contextlib.contextmanager
    def ncurses_win():
        curses_win = curses.initscr()
        try:
            yield curses_win
        finally:
            curses.endwin()

    def start(self):
        end_message = None

        with self.ncurses_win() as self.win:
            curses.noecho()
            delta = self.game_config.initial_delta
            last_delta = None
            while True:
                try:
                    passed = self._challenge_at_delta(delta)
                except UserQuitError:
                    passed = False
                if passed:
                    last_delta, delta = delta, delta/2.0
                elif last_delta is None:
                    end_message = "unable to determine pitch perception"
                    break
                else:
                    end_message = (
                        ("your ear is sensitive to pitch differences "
                         "up to {}Hz").format(delta*2))
                    break

        print (end_message)

if __name__ == "__main__":
    game_config = PitchPerceptionGameConfig()
    game = PitchPerceptionChallenge(game_config)
    game.start()
