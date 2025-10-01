-- # Larm - making sounds with Futhark
--
-- This is a small and rather crude Futhark library containing combinators for
-- describing sounds, which can then be sampled to produce audio files.
--
-- The library is largely a port of
-- [fpsynth](https://github.com/Ahnfelt/fpsynth) by Joakim Ahnfelt-Rønne and
-- Michael Werk Ravnsmed.
--
-- ## Usage
--
-- The following makes use of [Literate
-- Futhark](https://futhark.readthedocs.io/en/latest/man/futhark-literate.html).
-- See also [this blog post on generating audio with Literate
-- Futhark](https://futhark-lang.org/blog/2022-12-22-literate-audio.html).

import "lib/github.com/diku-dk/larm/larm"

def music : sound =
  scale 0.5 ((progressions.IxVxvixIV instruments.experimental
              `to` (progressions.IVxIxV instruments.experimental))
             `both` repeat 1 (progressions.bass instruments.bass))

def sample_music start end =
  sample start end 44100 music

-- > :audio sample_music 0 15;
-- codec: mp3

-- I do not know how to embed this in a GitHub README, but it sounds a bit like
-- [this](https://raw.githubusercontent.com/diku-dk/larm/refs/heads/main/README-img/1081966764298477b622ad879d6be9e5-output.mp3).
