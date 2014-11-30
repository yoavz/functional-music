module LittleStar where
import Haskore hiding (Major, Minor)
import AutoComp

times 1 m = m
times n m = m :+: times (n-1) m
mfd d n = n d melody_volume
melody_volume = [Volume 80]
harmony_volume = [Volume 40]

mA1 = lmap (mfd qn) [c 6, c 6, g 6, g 6, a 6, a 6] :+: mfd hn (g 6)
mA2 = lmap (mfd qn) [f 6, f 6, e 6, e 6, d 6, d 6] :+: mfd hn (c 6)
mA = mA1 :+: mA2

mB1 = lmap (mfd qn) [g 6, g 6, f 6, f 6, e 6, e 6] :+: mfd hn (d 6)
mB = times 2 mB1

melody = Instr "Lead 1 (square)" (mA :+: mB :+: mA)

cA :: ChordProgression
cA = map major [C, C, F, C, G, C, G, C]
cB = map major . (take 8) . cycle $ [C, G] 

littleChordProgression :: ChordProgression
littleChordProgression = cA ++ cB ++ cA

basicL = autoComp basic (C, Major) littleChordProgression
calypsoL = autoComp calypso (C, Major) littleChordProgression
boogieL = autoComp boogie (C, Major) littleChordProgression

littleStar = (Tempo 2 (melody :=: basicL))
