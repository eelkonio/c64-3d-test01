# c64-3d-test01
steps in recreating C64 fast drawing and 3d calculation algorithms


Ideas:

Create objects as follows:

;
; OBJECT:
; 
*=$abcd

planeDataPointer
  !word normalVectorCalculatedCheck
  !word planeData

normalVectors
  !byte x1, y1, z1
  !byte x2, y2, z2
...
  !byte xN, yN, zN
  !byte 0,0,0
normalVectorCalculatedCheck ; this is work-space for each vector
  !byte n1, n2, ..., nN
pointDataX ; this holds all X points of one object, at most 254 points
  !byte x1, x2, ...
pointDataY ; this holds all Y points of one object, at most 254 points
  !byte y1, y2, ...
pointDataZ ; this holds all Z points of one object, at most 254 points
  !byte z1, z2, ...
  ...
pointDataRecalculatedX ; this holds all X points of one object, at most 254 points
  !byte x1, x2, ...
pointDataRecalculatedY ; this holds all Y points of one object, at most 254 points
  !byte y1, y2, ...
pointDataRecalculatedZ ; this holds all Z points of one object, at most 254 points
  !byte z1, z2, ...
  ...
planeData
  !byte normalVectorIndex, pointDataIndexA, pointDataIndexQ, pointDataIndexF, ..., -1 (255)
  !byte normalVectorIndex, pointDataIndexR, pointDataIndexT, pointDataIndexV, ..., -1 (255)
  ...
  !byte -1 (255)

; Points need to be separated from the plane data. This prevents double point calculations,
; as they are used in different, adjoining planes.
