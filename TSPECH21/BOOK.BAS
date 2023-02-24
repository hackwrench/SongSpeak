'TÊEE co.(TM) Estonian speech synthesizer version 0.211
'SB part does not always work
'Go to  :  www.hot.ee/tmeeco   and go to the downloads section and
'download instructions to build yourself a LPTDAC. It works better
'than SB.

'###################### Modified to read text files #########################

'If you're an Estonian or perhaps Finnish then you can make this program
'speak English very well.

'you can contact me by sending E-mail to --- tmeeco@hot.ee ---

DECLARE SUB LOADSAMPLE (SAMPLEFILE$)
DECLARE SUB PLAYSAMPLE ()
DECLARE SUB PLAYSAMPLE2 ()
DECLARE SUB PLAYSAMPLE3 ()
DECLARE SUB READDSP (BYTE%)
DECLARE SUB RESETSBDSP (DSPRESET%)
DECLARE SUB TALK (TEXT$)
DECLARE SUB WRITEDSP (BYTE%)

COMMON SHARED SBBASE%, SBDMA%, DMAMASK%, DMAMODE%, DMACLEAR%, DMASTATUS%
COMMON SHARED DMAPAGE%, DMAADDR%, DMALEN%, DMAEND%, ROOT$, SAMPLE$, TEXT$
COMMON SHARED LPTBASE%, LPTSPEED%, PD%

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'IMPORTANT !!!! Change these if you want this thing working !!!!!!!!!!!!!
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

ROOT$ = "C:\WIN98SE\DESKTOP\SHIT\QB45\TSPEECH2\"     'path to files

' I'm too lazy to copy GETBLASTER stuff from one of my programs

SBBASE% = &H220         'SB base address
SBDMA% = 1              'SB DMA channel

                        'supported playback devices
PD% = 1                 'LPTDAC (0), SB DAC (1), PC-speaker (2)
                       
                        'LPTDAC sounds fine
                        'SB DAC sounds at the right speed
                        'PC-speaker as the last resort

LPTBASE% = &H378        'Base address of your LPT port (for LPTDAC users)
LPTSPEED% = 200         'Delay used to compensate machine's speed
                        '600 = Celeron @ 400MHz, 200 = Pentium @ 166MHz

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

IF PD% = 1 THEN
DMAMASK% = &HA: DMAMODE% = &HB
DMACLEAR% = &HC: DMASTATUS% = &H8
SELECT CASE SBDMA%
CASE 0: DMAPAGE% = &H87: DMAADDR% = 0: DMALEN% = 1: DMAEND% = 1
CASE 1: DMAPAGE% = &H83: DMAADDR% = 2: DMALEN% = 3: DMAEND% = 2
CASE 3: DMAPAGE% = &H82: DMAADDR% = 6: DMALEN% = 7: DMAEND% = 8
END SELECT

TIMEC% = 256 - 1000000 / 22000
WRITEDSP &H40   'Set time constant
WRITEDSP TIMEC%
END IF

SAMPLE1$ = SPACE$(8)

SCREEN 0: WIDTH 80, 25: CLS


OPEN "C:\win98se\desktop\a.txt" FOR BINARY AS #2


FOR i& = 0 TO LOF(2) / 80
TEXT$ = INPUT$(80, #2)
PRINT TEXT$
TALK TEXT$
NEXT i&
CLOSE #2
END

SUB LOADSAMPLE (SAMPLEFILE$)
OPEN SAMPLEFILE$ FOR BINARY ACCESS READ AS #8
IF LOF(8) = 0 THEN CLOSE #8
SAMPLELEN& = LOF(8) - 58
NOTREAD$ = INPUT$(58, #8)
IF SAMPLE% = 0 AND (FRE(-3) > SAMPLELEN&) THEN SAMPLE$ = INPUT$(SAMPLELEN&, #8)
CLOSE #8
END SUB

SUB PLAYSAMPLE

LENGHT% = LEN(SAMPLE$) - 1
'LOCATE 1, 1: PRINT LENGHT% + 1
SEGMENT& = VARSEG(SAMPLE$)
OFFSET& = SADD(SAMPLE$)
IF SEGMENT& < 0 THEN SEGMENT& = SEGMENT& + 65536
IF OFFSET& < 0 THEN OFFSET& = OFFSET& + 65536
DATAADDRESS& = SEGMENT& * 16 + OFFSET&
LOOK1% = VARPTR(DATAADDRESS&)
LOOK2% = VARPTR(LENGHT%)

WRITEDSP &HD0                   'halt current DMA operation

OUT DMAMASK%, SBDMA% + 4        'mask the DMA channel
OUT DMACLEAR%, &H0              'clear the internal DMA flip/flop
OUT DMAMODE%, 72 + SBDMA%       '72 = 010010XX  (XX = SBDMA%)
OUT DMAADDR%, PEEK(LOOK1%)      'bits 0-7 of  the 20bit address
OUT DMAADDR%, PEEK(LOOK1% + 1)  'bits 8-15 of the 20bit address
OUT DMAPAGE%, PEEK(LOOK1% + 2)  'bits 16-19 of the 20 bit address
OUT DMALEN%, PEEK(LOOK2%)       'bits 0-7 of size%
OUT DMALEN%, PEEK(LOOK2% + 1)   'bits 8-15  of size%
OUT DMAMASK%, SBDMA%            'enable channel

WRITEDSP &H14                   '8-bit PCM
WRITEDSP PEEK(LOOK2%)           'Low  byte of address
WRITEDSP PEEK(LOOK2% + 1)       'High byte of address

NOTHING% = INP(DMASTATUS%)      'Read status to make sure DMA is going.
T& = TIMER
DO WHILE INP(DMASTATUS%) <> DMAEND%:
IF TIMER - T& > .5 THEN EXIT DO
LOOP   'loop until sound is playied

END SUB

SUB PLAYSAMPLE2
FOR i& = 1 TO LEN(SAMPLE$)
BYTE$ = MID$(SAMPLE$, i&, 1)
FOR a% = 0 TO LPTSPEED%: NEXT a%
OUT LPTBASE%, ASC(BYTE$)
NEXT i&
END SUB

SUB PLAYSAMPLE3
'gives a shot at PC speaker
FOR i& = 1 TO LEN(SAMPLE$)
BYTE$ = MID$(SAMPLE$, i&, 1)
B% = ASC(BYTE$)
FOR a% = 0 TO LPTSPEED%: NEXT a%
IF B% > 127 THEN OUT &H61, 2
IF B% < 127 THEN OUT &H61, 0
NEXT i&
END SUB

SUB READDSP (BYTE%)
READTHEDSP:
WHILE INP((SBBASE% + &HE) AND &H80) = &H80: WEND
BYTE% = INP(SBBASE% + &HA)
IF BYTE% = &HAA THEN GOTO READTHEDSP
END SUB

SUB RESETSBDSP (DSPRESET%)
OUT SBBASE% + &H6, 1       'first reset step
OUT SBBASE% + &H6, 0       'second reset step
OLDTIME& = TIMER           'give 1 second to reset
WHILE (DSPRESET% = 0)      'if second passes then there's something wrong
STATUS% = INP(SBBASE% + &HA)
IF STATUS% = 170 THEN DSPRESET% = 1
IF INT(TIMER - OLDTIME&) >= 1 THEN DSPRESET% = 2
WEND
END SUB

SUB TALK (TEXT$)
TEXT$ = UCASE$(TEXT$)
FOR i% = 1 TO LEN(TEXT$)
CHAR$ = MID$(TEXT$, i%, 1)
SELECT CASE CHAR$
CASE "A": LOADSAMPLE ROOT$ + "A.WAV"
CASE "B": LOADSAMPLE ROOT$ + "B.WAV"
CASE "C": LOADSAMPLE ROOT$ + "C.WAV"
CASE "D": LOADSAMPLE ROOT$ + "D.WAV"
CASE "E": LOADSAMPLE ROOT$ + "E.WAV"
CASE "F": LOADSAMPLE ROOT$ + "F.WAV"
CASE "G": LOADSAMPLE ROOT$ + "G.WAV"
CASE "H": LOADSAMPLE ROOT$ + "H.WAV"
CASE "I": LOADSAMPLE ROOT$ + "I.WAV"
CASE "J": LOADSAMPLE ROOT$ + "J.WAV"
CASE "K": LOADSAMPLE ROOT$ + "K.WAV"
CASE "L": LOADSAMPLE ROOT$ + "L.WAV"
CASE "M": LOADSAMPLE ROOT$ + "M.WAV"
CASE "N": LOADSAMPLE ROOT$ + "N.WAV"
CASE "O": LOADSAMPLE ROOT$ + "O.WAV"
CASE "P": LOADSAMPLE ROOT$ + "P.WAV"
CASE "Q": LOADSAMPLE ROOT$ + "K.WAV": PLAYSAMPLE: LOADSAMPLE ROOT$ + "U.WAV"
CASE "R": LOADSAMPLE ROOT$ + "R.WAV"
CASE "S": LOADSAMPLE ROOT$ + "S.WAV"
CASE "T": LOADSAMPLE ROOT$ + "T.WAV"
CASE "U": LOADSAMPLE ROOT$ + "U.WAV"
CASE "V": LOADSAMPLE ROOT$ + "V.WAV"
CASE "W": LOADSAMPLE ROOT$ + "W.WAV"
CASE "X": LOADSAMPLE ROOT$ + "X.WAV"
CASE "Y": LOADSAMPLE ROOT$ + "Y.WAV"
CASE "Z": LOADSAMPLE ROOT$ + "Z.WAV"
CASE "Â": LOADSAMPLE ROOT$ + "Â.WAV"
CASE "ì": LOADSAMPLE ROOT$ + "Â.WAV"
CASE "é": LOADSAMPLE ROOT$ + "é.WAV"
CASE "Ñ": LOADSAMPLE ROOT$ + "é.WAV"
CASE "ô": LOADSAMPLE ROOT$ + "ô.WAV"
CASE "î": LOADSAMPLE ROOT$ + "ô.WAV"
CASE "ö": LOADSAMPLE ROOT$ + "ö.WAV"
CASE "Å": LOADSAMPLE ROOT$ + "ö.WAV"
CASE ELSE: LOADSAMPLE ROOT$ + "_.WAV"
END SELECT
'LOADSAMPLE ROOT$ + "-.WAV"
SELECT CASE PD%
CASE 0: PLAYSAMPLE2
CASE 1: PLAYSAMPLE
CASE 2: PLAYSAMPLE3
END SELECT
NEXT i%

END SUB

SUB WRITEDSP (BYTE%)
WHILE INP(SBBASE% + &HC) = 0: WEND
OUT SBBASE% + &HC, BYTE%
END SUB

