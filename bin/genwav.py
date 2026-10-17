import numpy as np
import scipy.io.wavfile as wav

# 1. Set the audio parameters
sample_rate = 44100  # Standard CD quality (samples per second)
duration = 4.0       # Duration in seconds
frequency = 256.0    # Frequency of Scientific Middle C in Hz

# 2. Generate the time axis array
t = np.linspace(0, duration, int(sample_rate * duration), endpoint=False)

# 3. Calculate the raw sine wave values
wave = np.sin(2 * np.pi * frequency * t)

# 4. Apply a fade envelope to prevent harsh starting/stopping pops
fade_samples = int(0.2 * sample_rate)  # 200 milliseconds fade
envelope = np.ones_like(wave)
envelope[:fade_samples] = np.linspace(0, 1, fade_samples)  # Fade in
envelope[-fade_samples:] = np.linspace(1, 0, fade_samples)  # Fade out
smooth_wave = wave * envelope

# 5. Convert floating numbers into 16-bit integer audio signals
audio_data = np.int16(smooth_wave * 32767)

# 6. Save the data arrays directly to a playable WAV file
wav.write('scientific_middle_c_256hz.wav', sample_rate, audio_data)
