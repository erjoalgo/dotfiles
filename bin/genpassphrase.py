#!/usr/bin/python
from __future__ import absolute_import
from __future__ import print_function
import argparse
import enum
import math
import random
from six.moves import map
from six.moves import range
from functools import reduce
from six.moves import input

parser = argparse.ArgumentParser()

class RNG(enum.Enum):
   PYTHON_RANDOM = "python-random" # use python standard library random
   DICE = "dice" # prompt for dice rolls


def read_number(prompt):
  num = eval(input(prompt))
  try:
    return int(num)
  except ValueError as ex:
    print (ex)
    return read_number(prompt)

def rng_choice(rng, choices, die_faces=6):
  if not choices:
    raise ValueError("no choices available")
  elif rng == RNG.PYTHON_RANDOM:
    return random.choice(choices)
  elif rng == RNG.DICE:
    rolls_needed = math.ceil(math.log(len(choices), die_faces))
    n = 0
    for i in range(rolls_needed):
      d = read_number("enter {}/{} dice roll (1-{}): ".format(
          i+1, rolls_needed,
          die_faces))
      if not (1 <= d <= die_faces):
        raise ValueError("expected die roll value in range [1, {}], not {}".format(
            die_faces, d))
      n = n*die_faces + (d-1)
    assert 0 <= n < len(choices)
    return choices[n]
  else:
    raise ValueError("invalid RNG '{}'. expected one of {}".format(
                     rng, [a.value for a in RNG]))

def read_file_words(filename):
  with open(filename, "r") as fh:
    text = fh.read()
    words = text.split("\n")
    return words


if __name__ == "__main__":
  parser.add_argument("-n", "--num_words", help = "number of words", type=int,
                      default = 8)
  parser.add_argument("-f", "--dict_files",
                      help = "text file of newline-delimited words",
                      nargs="+",
                      default=["/var/lib/dictionaries-common/aspell/aspell-en"])
  parser.add_argument("-r", "--rng", help = "random number generator to use",
                      choices=[a.value for a in RNG], default=RNG.PYTHON_RANDOM,
                      type=RNG)
  parser.add_argument("-d", "--die_faces", help = "the number of faces in the die",
                      default=6, type=int)

  args=parser.parse_args()
  words = reduce(lambda a, b: a + b, list(map(read_file_words, args.dict_files)))
  if not words:
    raise ValueError("no words found in {}".format(
        "\n".oin(args.dict_files)))
  passphrase_words = []
  for i in range(args.num_words):
    word = rng_choice(args.rng, words, die_faces=args.die_faces)
    assert word
    passphrase_words.append(word)

  print((" ".join(passphrase_words)))
