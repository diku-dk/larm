-- | Noisemaking library.
--
-- Very crude: in particular, prone to clipping.

-- | A point in time, in seconds. Should be non-negative.
type time = f32

-- | The amplitude of the sound wave - should be between -1 and 1.
type amplitude = f32

-- | A duration in time, in seconds. Should be non-negative.
type duration = f32

-- | Frequency in kHz.
type kHz = f32

-- | Frequency in Hz.
type Hz = f32

local
def arange (start: f32) (stop: f32) (step: f32) : []f32 =
  let n = f32.max 0 (f32.floor ((stop - start) / step))
  in tabulate (i64.f32 n) (\i -> start + f32.i64 i * step)

-- | Representation of a sound with associated duration information.
--
-- The `gen` field is a function from time to amplitude, i.e., the function that
-- actually generates the sound. The `duration` plus `overlap` is the amount of
-- time that this sound is non-silent - it is allowed for this to be infinite.
-- The division into `dur` and `overlap` is used by combinators to overlap
-- sounds in a hopefully pleasant way - this is a bit ad-hoc.
type^ sound =
  { gen: time -> amplitude
  , dur: duration
  , overlap: duration
  }

-- | Play both sounds simultaneously.
def both (a: sound) (b: sound) : sound =
  { gen = \t -> a.gen t + b.gen t
  , dur = f32.max a.dur b.dur
  , overlap = f32.max a.overlap b.overlap
  }

-- | Repeat this sound at a rate of `rate` per second.
def repeat rate (s: sound) : sound =
  { gen = \t -> s.gen (t % (1 / rate))
  , dur = f32.inf
  , overlap = 0
  }

-- | First play one sound, then the other. The second sound starts playing after
-- the `dur` of the first one has passed, but it is still playing until
-- `overlap` has also passed.
def to (a: sound) (b: sound) : sound =
  { gen =
      \t ->
        let a' =
          if t < a.dur + a.overlap
          then a.gen t
          else 0
        let b' =
          if t > a.dur && (t - a.dur) < b.dur + b.overlap
          then b.gen (t - a.dur)
          else 0
        in a' + b'
  , dur = a.dur + b.dur
  , overlap = b.overlap
  }

-- | Delay a sound by some duration.
def delay (d: duration) (s: sound) : sound =
  { gen = \t -> s.gen (t - d)
  , dur = s.dur + d
  , overlap = s.overlap
  }

-- | Scale the volumne of a sound.
def scale (x: f32) (s: sound) : sound =
  { gen = \t -> x * s.gen t
  , dur = s.dur
  , overlap = s.overlap
  }

-- | Create a sound of infinite duration and zero overlap.
def tone (f: time -> amplitude) : sound =
  {gen = \t -> f t, dur = f32.inf, overlap = 0}

-- | Various primitive waveforms. Note that these are not of type `sound`@term,
-- but straight up functions from time to amplitude.
module waveforms = {
  def sine (frequency: Hz) (t: time) : amplitude =
    let phase = t * frequency * 2 * f32.pi
    in f32.sin phase

  def triangle (frequency: Hz) (t: time) : amplitude =
    let phase = t * frequency * 2.0 * f32.pi
    let p = phase / (2.0 * f32.pi)
    in 2.0 * f32.abs (2.0 * (p - f32.floor (p + 0.5))) - 1.0

  def square (frequency: Hz) (t: time) : amplitude =
    let phase = t * frequency * 2.0 * f32.pi
    in f32.sgn (f32.sin phase)

  def sawtooth (frequency: Hz) (t: time) : amplitude =
    let phase = t * frequency * 2.0 * f32.pi
    let p = phase / (2.0 * f32.pi)
    in 2 * (p - f32.floor (p + 0.5))

  def smoothtooth (frequency: Hz) (t: time) : amplitude =
    let phase = t * frequency * 2 * f32.pi
    let t' = t + f32.sin phase / (frequency * 2 * f32.pi)
    let newPhase = t' * frequency * 2 * f32.pi
    in f32.sin newPhase

  def better_smoothtooth (frequency: Hz) (t: time) : amplitude =
    let phase = t * frequency * 2 * f32.pi
    let t' = t + f32.sin phase / (frequency * 2 * f32.pi) * 2
    let newPhase = t' * frequency * 2 * f32.pi
    in f32.sin newPhase

  def continuous_noise (frequency: Hz) (t: time) : amplitude =
    let t' = t + smoothtooth t frequency
    let newPhase = t' * frequency * 2 * f32.pi
    in f32.sin newPhase

  local def fract (x: f32) : f32 = x - f32.floor x

  def random_noise (t: time) : amplitude =
    let s = f32.sin (t * 12.9898)
    let v = fract (s * 43758.5453)
    in 2.0 * v - 1.0
}

-- | The [ADSR envelope](https://en.wikipedia.org/wiki/Envelope_(music)#ADSR)
-- with all parameters exposed.
def adsr' (duration: duration)
          (attack: duration)
          (decay: duration)
          (sustain: duration)
          (release: duration)
          (amplitude: time -> amplitude) : sound =
  { gen =
      \t ->
        if t < 0.0
        then 0.0
        else if t < attack
        then amplitude t * (t / attack)
        else if t < attack + decay
        then amplitude t * (1.0 - (1.0 - sustain) * (t - attack) / decay)
        else if t < duration
        then amplitude t * sustain
        else if t < duration + release
        then amplitude t * sustain * (1.0 - (t - duration) / release)
        else 0.0
  , dur = duration
  , overlap = release
  }

-- | The ADSR envelope with attack, decay, sustain, and release set to pleasant
-- defaults.
def adsr (duration: f32) (amplitude: (time -> amplitude)) : sound =
  adsr' duration 0.1f32 0.2f32 0.7f32 0.2f32 amplitude

local
def softclip (s: time -> amplitude) (t: time) : amplitude =
  let a = s t in a - (a * a * a / 3.0)

-- | Various effects.
module effects = {
  def harmonic (sound: (time -> amplitude)) (t: time) : amplitude =
    sound t + 0.3 * sound (2 * t) + 0.1 * sound (3 * t)

  -- | Make the sample at a given time the average of the samples in a *(-r,r)*
  -- span around that time, itself sampled at the given rate. Note that this is
  -- a fairly expensive effect.
  def avg (r: f32) (rate: kHz) (s: sound) : sound =
    { gen =
        \t ->
          let pts = arange (t - r) (t + r) (r * 2 / rate)
          in f32.sum (map s.gen pts)
             / f32.i64 (length pts)
    , dur = s.dur + r
    , overlap = s.overlap
    }

  -- | Make a sound whose amplitude is the derivative of an underlying sound.
  -- This usually sounds horrible.
  def diff (s: sound) : sound =
    s with gen = \t -> jvp s.gen t 1
}

-- | Various instruments.
module instruments = {
  def snare (duration: duration) (frequency: kHz) : sound =
    let pitch_envelope t = 1.0f32 + 2.0f32 * f32.exp (-t * 80.0f32)
    let body =
      adsr' duration
            0.001
            0.07
            0.0
            0.0
            (\t -> waveforms.sine (frequency * pitch_envelope t) t)
    let snare =
      adsr' duration
            0.0
            0.15
            0.0
            0.0
            waveforms.random_noise
    in scale 0.6 body `both` scale 0.8 snare

  def kick (duration: duration) (frequency: kHz) : sound =
    let pitch_envelope t = 1.0 + 1.5 * f32.exp (-t * 20.0)
    let body =
      adsr' duration
            0.001
            0.2
            0.0
            0.0
            (\t -> waveforms.sine (frequency * pitch_envelope t) t)
    let punch =
      adsr' duration
            0.0
            0.05
            0.0
            0.0
            (softclip (\t -> 0.3 * waveforms.sine (frequency * pitch_envelope t * 2.0) t))
    in body `both` punch

  def hihat (duration: duration) : sound =
    let hat =
      adsr' duration
            0.001
            0.05
            0.0
            0.0
            waveforms.random_noise
    let metallic =
      adsr' duration
            0.001
            0.05
            0.0
            0.0
            (\t -> 0.3 * waveforms.triangle 8000.0 t)
    in hat `both` metallic

  def smoothtooth (duration: duration) (frequency: kHz) : sound =
    adsr duration (waveforms.smoothtooth frequency)

  def experimental (duration: duration) (frequency: kHz) : sound =
    let f t =
      waveforms.smoothtooth frequency t
      + waveforms.sine (frequency / 2.0) t
    in adsr' duration
             0.01
             0.5
             0.5
             0.3
             (softclip (effects.harmonic f))

  def bass (duration: duration) (frequency: kHz) : sound =
    adsr' duration
          0.02
          0.2
          0.25
          0.2
          (softclip (effects.harmonic (effects.harmonic \t ->
                                         waveforms.smoothtooth frequency t
                                         + waveforms.sine (frequency / 2.0f32) t)))
}

-- | Various midi notes.
module midi = {
  def note (octave: i32) (semitone: f32) : kHz =
    let note = 12.0 * f32.i32 (octave + 1) + semitone
    in 440.0 * 2 ** ((note - 69.0) / 12.0)

  def C (octave: i32) : kHz = note octave 0.0
  def Cs (octave: i32) : kHz = note octave 1.0
  def Db (octave: i32) : kHz = Cs octave
  def D (octave: i32) : kHz = note octave 2.0
  def Ds (octave: i32) : kHz = note octave 3.0
  def Eb (octave: i32) : kHz = Ds octave
  def E (octave: i32) : kHz = note octave 4.0
  def F (octave: i32) : kHz = note octave 5.0
  def Fs (octave: i32) : kHz = note octave 6.0
  def Gb (octave: i32) : kHz = Fs octave
  def G (octave: i32) : kHz = note octave 7.0
  def Gs (octave: i32) : kHz = note octave 8.0
  def Ab (octave: i32) : kHz = Gs octave
  def A (octave: i32) : kHz = note octave 9.0
  def As (octave: i32) : kHz = note octave 10.0
  def Bb (octave: i32) : kHz = As octave
  def B (octave: i32) : kHz = note octave 11.0
}

-- | Some standard chord progressions.
module progressions = {
  def IxVxvixIV (play: (duration -> kHz -> sound)) : sound =
    (play 0.5 (midi.C 4) `both` play 0.5 (midi.E 4))
    `to` (play 0.5 (midi.G 4))
    `to` (play 0.5 (midi.B 3) `both` play 0.5 (midi.D 4))
    `to` play 0.5 (midi.G 4)
    `to` (play 0.5 (midi.C 4) `both` play 0.5 (midi.E 4))
    `to` (play 0.5 (midi.A 4))
    `to` (play 0.5 (midi.C 4) `both` play 0.5 (midi.F 4))
    `to` play 0.5 (midi.A 4)

  def IVxIxV (play: (duration -> kHz -> sound)) : sound =
    (play 0.5 (midi.C 4) `both` play 0.5 (midi.E 4))
    `to` play 0.5 (midi.A 4)
    `to` (play 0.5 (midi.C 4) `both` play 0.5 (midi.F 4))
    `to` play 0.5 (midi.A 4)
    `to` (play 0.5 (midi.C 4) `both` play 0.5 (midi.E 4))
    `to` (play 0.5 (midi.G 4))
    `to` (play 0.5 (midi.B 3) `both` play 0.5 (midi.D 4))
    `to` play 0.5 (midi.G 4)

  def bass (play: (duration -> kHz -> sound)) : sound =
    let multi (frequency: kHz): sound =
      play 1.0f32 frequency
      `both` delay 0.5 (play 0.5f32 frequency)
    in multi (midi.C 3)
       `to` multi (midi.B 2)
       `to` multi (midi.C 3)
       `to` multi (midi.C 3)
       `to` multi (midi.C 3)
       `to` multi (midi.C 3)
       `to` multi (midi.C 3)
       `to` multi (midi.B 2)
}

-- | Sample a sound in a given range and with a given sampling frequency. 44100
-- is a good frequency to use.
def sample (start: f32) (end: f32) (hz: i64) (s: sound) =
  map s.gen (arange start end (1 / f32.i64 hz))
