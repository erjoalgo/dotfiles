#!/usr/bin/python

# adapted from: https://stackoverflow.com/questions/33879523/
# adapted from: www.daniweb.com/code/snippet263775.html

import argparse
import math
import struct
import wave

class AudioFile(object):
    def __init__(self, sample_rate = 44100.0):
        # Audio will contain a long list of samples (i.e. floating point numbers describing the
        # waveform).  If you were working with a very long sound you'd want to stream this to
        # disk instead of buffering it all in memory list this.  But most sounds will fit in
        # memory.
        self.audio = []
        self.sample_rate = sample_rate

    def append_silence(self, duration_milliseconds=500):
        """
        Adding silence is easy - we add zeros to the end of our array
        """
        num_samples = duration_milliseconds * (self.sample_rate / 1000.0)
        for x in range(int(num_samples)):
            self.audio.append(0.0)


    def append_sinewave(self,
            freq=440.0,
            duration_milliseconds=500,
            volume=1.0):
        """
        The sine wave generated here is the standard beep.  If you want something
        more aggresive you could try a square or saw tooth waveform.   Though there
        are some rather complicated issues with making high quality square and
        sawtooth waves... which we won't address here :)
        """
        num_samples = duration_milliseconds * (self.sample_rate / 1000.0)
        for x in range(int(num_samples)):
            sample = volume * math.sin(
                2 * math.pi * freq * ( x / self.sample_rate ))
            self.audio.append(sample)

    def save_wav(self, file_name,
                 # wav params
                 nchannels = 1,
                 sampwidth = 2):
        # Open up a wav file
        with open(file_name, "w") as wav_file:
            wav_file=wave.open(file_name,"w")
            # 44100 is the industry standard sample rate - CD quality.  If you need to
            # save on file size you can adjust it downwards. The stanard for low quality
            # is 8000 or 8kHz.
            nframes = len(self.audio)
            comptype = "NONE"
            compname = "not compressed"
            wav_file.setparams((nchannels, sampwidth, self.sample_rate, nframes, comptype, compname))

            # WAV files here are using short, 16 bit, signed integers for the
            # sample size.  So we multiply the floating point data we have by 32767, the
            # maximum value for a short integer.  NOTE: It is theortically possible to
            # use the floating point -1.0 to 1.0 data directly in a WAV file but not
            # obvious how to do that using the wave module in python.
            for sample in self.audio:
                wav_file.writeframes(struct.pack('h', int( sample * 32767.0 )))


def write_test_up_down():
    for (second_freq, file_name) in [(880, "up.wav"), (330, "down.wav")]:
        audio = AudioFile()
        audio.append_sinewave(volume=0.25, freq=440,
                              duration_milliseconds=100)
        # append_silence()
        audio.append_sinewave(volume=0.5, freq=second_freq,
                              duration_milliseconds=100)
        # append_silence()
        # append_sinewave()
        audio.save_wav(file_name)



def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("spec",
                        nargs="+",
                        help="A list of FREQUENCY[:DURATION_MS[:VOLUME]] specs.")
    parser.add_argument("--output", help="output filename")
    args = parser.parse_args()
    audio = AudioFile()
    class SoundSpec(object):

        def __init__(self, freq, duration_ms=250, volume=.5):
            self.freq = freq
            self.duration_ms = duration_ms
            self.volume = volume

    for spec in args.spec:
        spec = SoundSpec(*map(float, spec.split(":")))
        if spec.freq > 0:
            audio.append_sinewave(freq=spec.freq,
                                  duration_milliseconds=spec.duration_ms,
                                  volume=spec.volume)
        else:
            audio.append_sinewave(duration_milliseconds=spec.duration_ms)
    audio.save_wav(args.output)


if __name__ == "__main__":
    main()
