Introduction
============

In this code essay we will be using the Haskore library and some Data.List functions. We will hide the Key, Major, and Minor labels because we would like to reserve those labels for our own namespace.

> module AutoComp where
> import Haskore hiding (Key, Major, Minor)
> import Data.List

lmap is a useful utility function that maps a function over a list and creates a Haskore music sequence from it. 

> lmap f l = line (map f l)

We also define default volumes to be used when generating our basslines and chord voicings.

> defaultChordVolume = [Volume 80]
> defaultBassVolume = [Volume 80]


Basic Data Structures and Types
===============================

To begin our musical explorations, we define a few general basic types and data structures. A key consists of two parts: a root pitch and a harmonic quality. A harmonic quality may either be Major or Minor.

> data HarmonicQuality = Major | Minor deriving (Show, Eq)
> type Key = (PitchClass, HarmonicQuality)

> keyRoot :: Key -> PitchClass
> keyRoot = fst
> keyQuality :: Key -> HarmonicQuality 
> keyQuality = snd

A scale pattern is a list of integers representing the distance in half-steps from root. For example: ionian (major) is [0, 2, 4, 5, 7, 9, 11]. 

> type ScalePattern = [Int]

> ionian, lydian, mixolydian, aeolian, dorian, phrygian :: ScalePattern 
> ionian = [0, 2, 4, 5, 7, 9, 11]
> lydian = [0, 2, 4, 6, 7, 9, 11]
> mixolydian = [0, 2, 4, 5, 7, 9, 10]
> aeolian = [0, 2, 3, 5, 7, 8, 10]
> dorian = [0, 2, 3, 5, 7, 9, 10]
> phrygian = [0, 1, 3, 5, 7, 8, 10]

Next, we define two types. A Scale is a subset of pitches (generalized to any octave). An example scale would be [C, D, E, F, G, A, B]. 

> type Scale = [PitchClass]

A Chord Scale consists of a root pitch and a scale pattern, and can be used to generate a Scale. We define several functions that assist with generating Scales from a ChordScale and other inputs.

> type ChordScale = (PitchClass, ScalePattern)

> chordScaleRoot :: ChordScale -> PitchClass
> chordScaleRoot = fst
> chordScalePattern :: ChordScale -> ScalePattern
> chordScalePattern = snd 

> generateScale :: ChordScale -> Scale
> generateScale scale = map combineNotes (zip (repeat (chordScaleRoot scale)) (chordScalePattern scale))
>   where combineNotes (root, step) = fst (pitch ((pitchClass root) + step))

> generateMajorScale, generateMinorScale :: PitchClass -> Scale
> generateMajorScale r = generateScale (r, ionian)
> generateMinorScale r = generateScale (r, aeolian) 

> generateKeyScale :: Key -> Scale 
> generateKeyScale (p, Major) = generateMajorScale p
> generateKeyScale (p, Minor) = generateMinorScale p

A Chord consists of a root pitch and a harmonic quality.

> type Chord = (PitchClass, HarmonicQuality)

> major, minor :: PitchClass -> Chord
> major p = (p, Major)
> minor p = (p, Minor)

> chordRoot :: Chord -> PitchClass
> chordRoot = fst
> chordQuality :: Chord -> HarmonicQuality
> chordQuality = snd

We need some way of mapping a Chord to it's respective position in a key. First, we will define the equivalentPitches, which returns true if the pitches are equivalent to each other. We need this function instead of using the (==) operator because there may be some PitchClasses that are different but represent the same tone. For example, the expression "Ds == Ef" will return false, but we would like "Ds `equivalentPitches` Ef" to return true. Fortunately, Haskore has a built in pitchClass function which makes this comparison simple; we only have to watch out for the special case of C and Bs.

> equivalentPitches :: PitchClass -> PitchClass -> Bool
> equivalentPitches C Bs = True
> equivalentPitches Bs C = True
> equivalentPitches a b = (==) (pitchClass a) (pitchClass b)

Now we can define calculatePosition, which returns the relative position of a chord root in a key from 1-7. If the chord root is not in the key scale, then calculatePosition returns 0. Let's also go ahead and create a type for relative position (Pos) for clarity.

> type Pos = Int

> calculatePosition :: Key -> Chord -> Pos
> calculatePosition k c = 
>   maybe 0 (+1) (findIndex (equivalentPitches $ chordRoot c) (generateKeyScale k)) 

calculateScalePattern encodes the table in the assignment description. It returns a scale pattern given a relative position and harmonic quality of a chord. 

> calculateScalePattern :: Pos -> HarmonicQuality -> ScalePattern
> calculateScalePattern 1 Major = ionian
> calculateScalePattern 2 Major = mixolydian
> calculateScalePattern 2 Minor = dorian
> calculateScalePattern 3 Minor = phrygian 
> calculateScalePattern 4 Major = lydian 
> calculateScalePattern 5 Major = mixolydian 
> calculateScalePattern 6 Minor = aeolian
> calculateScalePattern _ _ = error "No scale pattern defined for that chord class"

With the functions defined so far we can now generate a chord scale for a given song key and chord class. Given a Key and a Chord, calculateChordScale returns the relevant scale of notes. This function may also generate an error if the root pitch of the chord is not found within the key; this error will happen if a chord in the progression is inconsistent with the key of the song. 

> calculateChordScale :: Key -> Chord -> Scale 
> calculateChordScale k c 
>   | pos == 0 = error "Chord root pitch not found in the key"
>   | otherwise = generateScale (chordRoot c, scalePattern)
>   where pos = (calculatePosition k c)
>         scalePattern = calculateScalePattern pos (chordQuality c)

Generating Basslines
====================

We have now built up enough general functions to work towards generating a bass note given a key, chord, position, duration. The bass note is generated in an octave range 3-4, always generated above the root pitch of the chord. 

> generateBassNote :: Key -> Chord -> Pos -> Dur -> Music 
> generateBassNote k c p d = Note (pitch, octave) d defaultBassVolume 
>   where pitch = (!!) (calculateChordScale k c) (p-1)
>         octave = if pitch < (chordRoot c) then 4 else 3 

A BassStyle is a list of tuples, where each tuple encodes a relative position and duration. Each list represents one measure of bassline, so the durations of each tuple list should add up to one measure. We also need a way to encode rests in our bassline, so let us introduce the convention that a position of 0 represents a rest.

> type BassStyle = [(Pos, Dur)]

We can now define three bass styles: basic, calypso, and boogie. Remember that a position of 0 represents a rest.

> basic, calypso, boogie :: BassStyle
> basic = [(1, hn), (5, hn)]
> calypso = take 6 . cycle $ [(0, qn), (1, en), (3, en)]
> boogie = take 8 . cycle $ [(1, en), (5, en), (6, en), (5, en)]

A chord progression is a list of chords. By convention, each chord will represent a half measure, so a chord progression list of length 2N represents the chord progression of a song over N measures.

> type ChordProgression = [Chord]

Now we are ready to define a key function. Given a bass style, key, two chords (representing a chord progression over one measure), generateBassMeasure generates the appropriate bassline for that measure. Implementation note: this function assumes that a BassStyle can be split into two lists of equal length, each representing half a measure.

> generateBassMeasure :: BassStyle -> Key -> (Chord, Chord) -> Music

> generateBassMeasure s k c = 
>   lmap (genNote $ fst c) fstHalf :+: lmap (genNote $ snd c) sndHalf
>   where nHalf f = flip f s . (flip quot 2) . length $ s 
>         fstHalf = nHalf take
>         sndHalf = nHalf drop
>         genNote _ (0, dur) = Rest dur
>         genNote c (pos, dur) = generateBassNote k c pos dur

Now we're ready to define autoBass. All we need is to map the chord progression to a series of chord tuples, each representing a measure. Then we can lmap generateBassMeasure with the appropriate style and key over the measure tuples of the chord progression. We pair the resulting Haskore Music object with the "Acoustic Bass" MIDI instrument.

> listToTuples :: [a] -> [(a, a)]
> listToTuples [] = []
> listToTuples (x:y:ys) = (x, y) : listToTuples ys

> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass s k p = Instr "Acoustic Bass" (lmap (generateBassMeasure s k) prog)
>   where prog = listToTuples p

Generating Chords 
=================

To represent concrete chords, we define Triad, which is a list of pitch classes. Note that although this type is technically equivalent to Scale, the list of PitchClass's should be a subset of a Scale. An example of a Triad would be [C, E, G] whereas its corresponding Scale is the full 7 pitch classes of C Major: [C, D, E, F, G, A, B]

> type Triad = [PitchClass]

Given a key and a chord, calculateChord returns the appropriate triad. This is done by calculating chord scale using the same technique above, then taking the 1st, 3rd, and 5th positions of that scale.

> calculateChord :: Key -> Chord -> Triad
> calculateChord k c = map (scale !!) [0, 2, 4]
>   where scale = calculateChordScale k c

ChordPattern represents the list of Pitches that will make up a chord. Note that this type is different from Triad in that it includes octave information whereas Triad does not.

> type ChordPattern = [Pitch]

generatePatternSimple generates a ChordPattern given a Chord and the Key of the song. For now, we'll work towards developing a simplified version of autoChord, which places the chord notes in a default chord octave. In the next section, we will improve our solution to adhere to chord voicing guidelines.

> defaultChordOctave = 5

> generatePatternSimple :: Key -> Chord -> ChordPattern
> generatePatternSimple k c = [(x, defaultChordOctave) | x <- calculateChord k c] 

From this point onwards, it is fairly simple to define autoChordSimple; just define a generateChord function to generate Haskore music objects from Chords given the song key, then lmap the generateChord function over the entire chord progression.

> generateChordSimple :: Key -> Chord -> Music
> generateChordSimple k c = 
>   chord [ Note x hn defaultChordVolume | x <- generatePatternSimple k c ] 

> autoChordSimple :: Key -> ChordProgression -> Music
> autoChordSimple k = Instr "piano" . lmap (generateChordSimple k)

Chord Voicing
=============

The next step is to generate smarter chord voicing. Specifically, we would like to adhere to the following two guidelines in our harmony:

1. Closeness: Make the changes in the chord melodies as small as possible. Concretely, the change in the melody lines should be minimized

2. Tightness: The chord should be kept as tight as possible by keeping the tones in the triad as close to each other as possible.

We will approach this problem by creating functions that generate a score for Closeness and Tightness. We will also produce a function that generates a list of all the possible 8 ChordPatterns for a given Triad using the octave range 4-5. We can then loop over this list of ChordPatterns and choose the most optimal using our score functions.

generatePossiblePatterns takes a triad and generates all possible chord patterns in the octave range 4-5.

> generatePossiblePatterns :: Triad -> [ChordPattern]
> generatePossiblePatterns [] = [[]]
> generatePossiblePatterns (c:cs) = 
>   [ x:y | x <- [(c, 4), (c, 5)], y <- generatePossiblePatterns cs ]

Next, we need to write our score functions. Tightness is simpler since it only requres the input of a ChordPattern, so let us start with that one. chordTightness calculates a tightness score by returning the semitone difference between the lowest and the highest pitch of the chord pattern. Remember, this means we want a chord pattern with a **low** tightness score.

> chordTightness :: ChordPattern -> Int
> chordTightness p = (maximum pitches) - (minimum pitches)
>   where pitches = map absPitch p

chordCloseness is a little tricker; it takes two chord patterns, sorts them to ensure they are in musical ascending order, then sums the differences between each absolute pitch. Again, this means we want a chord pattern with a **low** closeness score when compared to the previous chord pattern in the harmony.

> chordCloseness :: ChordPattern -> ChordPattern -> Int
> chordCloseness a b = sum . map abs $ zipWith (-) pA pB 
>   where pA = sort . map absPitch $ a
>         pB = sort . map absPitch $ b

To find the most optimal chord over the list of all possible chords, we would like to use the maximumBy function in the Data.List package. This function takes as input an ordering function (a -> a -> Ordering), and a list [a] to return the optimal a. In our case, a should be ChordPattern. To use maximumBy, we just need to define an ordering function.

There is one subtlety with the ordering function: the chord being compared against for a closeness score changes with every iteration. That is, on one iteration our chord voicing algorithm may need to compare the closeness of a and b to (C, Major), but on the next it may need to compare the closeness of d and c to (G, Major). This sounds like a great use case for function currying!

chordOrdering takes as input a target ChordPattern (representing the previous chord pattern in the progression) and produces an ordering function (ChordPattern -> ChordPattern -> Ordering) that can be used to choose the optimal chord pattern. It does this by combining the closeness (relative to target) and tightness score of each possible chord. Each score is multiplied by a constant factor that can be tweaked to the programmer's liking to produce different voicing outcomes. Remember that we would like the **lowest** possible tightness and closeness scores, so a lower combination of them results in a higher ordering.

> cFactor :: Int
> cFactor = 2

> tFactor :: Int
> tFactor = 1

> chordOrdering :: ChordPattern -> (ChordPattern -> ChordPattern -> Ordering)
> chordOrdering target a b 
>   | dA < dB = GT
>   | dA == dB = EQ
>   | dA > dB = LT
>   where score n = cFactor * (chordCloseness target n) + tFactor * (chordTightness n) 
>         dA = score a 
>         dB = score b 

Now we can define a smarter generatePattern function. It is similar to generatePatternSimple but takes as input one extra parameter: the previous chord pattern. Using the previous chord pattern and the above functions, we can return an optimal next chord pattern for a given key and chord.

> generatePattern :: Key -> Chord -> ChordPattern -> ChordPattern
> generatePattern k c prev = maximumBy (chordOrdering prev) possible 
>   where possible = generatePossiblePatterns $ calculateChord k c

> generateChord :: Key -> Chord -> ChordPattern -> Music
> generateChord k c prev = 
>   chord [ Note x hn defaultChordVolume | x <- generatePattern k c prev ]

Finally, we can write our final autoChord function. This function will use an auxiliary function that takes as input the key, chord progression, and the previous chord pattern. If it is the first chord in the progression (represented by an empty ChordPattern for the prev argument), the chord is generated in the default octave range using the simple functions defined in the last section. Otherwise, the chord is generated using the smarter functions of this section. On every iteration, the calculated chord pattern is passed to the next iteration of the function.

> autoChordAux :: Key -> ChordProgression -> ChordPattern -> Music
> autoChordAux _ [] _ = Rest qn
> autoChordAux k (p:ps) [] = generateChordSimple k p :+: autoChordAux k ps pattern 
>   where pattern = generatePatternSimple k p 
> autoChordAux k (p:ps) prev = generateChord k p prev :+: autoChordAux k ps pattern
>   where pattern = generatePattern k p prev

> autoChord :: Key -> ChordProgression -> Music
> autoChord k p = Instr "piano" (autoChordAux k p [])

Now that we have autoChord and autoBass functions, we can put it all together into an autoComp function:

> autoComp :: BassStyle -> Key -> ChordProgression -> Music
> autoComp style key prog = autoBass style key prog :=: autoChord key prog
