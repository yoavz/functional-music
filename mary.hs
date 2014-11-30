module MaryLamb where
import Haskore hiding (Major, Minor)
import AutoComp 

melody_volume = [Volume 80]
mfd d n = n d melody_volume

cadence n1 n2 n3 = lmap (mfd qn) [n1, n2] :+: mfd hn n3 
mA1 = lmap (mfd qn) [b 5, a 5, g 5, a 5]
mA2 = cadence (b 5) (b 5) (b 5)
mA3 = cadence (a 5) (a 5) (a 5)
mA4 = cadence (b 5) (d 6) (d 6)
mA5 = lmap (mfd qn) [a 5, a 5, b 5, a 5] :+: mfd wn (g 5)

mA = line [mA1, mA2, mA3, mA4, mA1, mA2, mA5]

maryChordProgression = take 16 . cycle $ [(G, Major), (G, Major), 
                                          (G, Major), (G, Major),
                                          (D, Major), (D, Major), 
                                          (G, Major), (G, Major)]

melody = Instr "Lead 1 (square)" mA 

basicM = autoComp basic (G, Major) maryChordProgression
calypsoM = autoComp calypso (G, Major) maryChordProgression
boogieM = autoComp boogie (G, Major) maryChordProgression

maryLamb = (Tempo 3 (melody :=: boogieM))
