$Unstable:Midi
$MidiSoundFont:Default
'Rewritten by Robert Claypool for QB64, using code and mainly sound samples from
'                   ‹€‹     ﬂ€ﬂ
'                  ‹€ €‹     €
'***************  ‹€‹‹‹€‹    €               ******************************
'                 €     € ‹ ‹€‹ ‹ ENGINE 1.0

'                     By StevenM and Raze, June 2000

'StevenM: StevenM86@aol.com         Raze: andrew_raze@telstra.easymail.com.au
'_____________________________________________________________________________

'
Type ByteArray
    Byte As String * 1
End Type

DECLARE SUB say (e$)
DECLARE FUNCTION Capital% (l$)
DECLARE SUB delay (tdelay!)
DECLARE SUB GuessWord (WS$)
DECLARE SUB NewWord (w$)
DECLARE FUNCTION NUMWORD$ (n%)

DECLARE FUNCTION WordArray! (WORD$)
'$DYNAMIC
DECLARE SUB userinp (userkey$)

Dim Shared MaxChan%
Dim Shared SndChan(0) As String


Dim Shared SCHandle(0 To 255) As Long
Dim Shared VoiceChannel As Integer

SetMaxChan 2
VoiceChannel = 1
SCList$ = "aA12345bcdeEfgGhiIjklmnNoO67prsStTHuU89vwWz0"

For B = 1 To Len(SCList$)
    S$ = Mid$(SCList$, B, 1)
    If Capital(S$) Then
        SCHandle(Asc(S$)) = LoadSound("V1c" + S$ + ".wav")
    Else
        SCHandle(Asc(S$)) = LoadSound("v1"+S$ + ".wav")
    End If
Next B
For B = 0 To 255
    If SCHandle(B) = 0 Then SCHandle(B) = SCHandle(Asc("0"))
Next




'----
Dim Shared Words(1000) As String 'Space for 1000 words.
Dim Shared Speech(1000) As String
'                 '| increase this if you want more word space.
Cls

'S$ = "EnESalIN" 'Say INITIALIZING
'FOR aaa = 1 TO LEN(S$)
'EnqueSound SCHandle(ASC(MID$(S$, aaa, 1))), VoiceChannel
'NEXT aaa


Print "***** Initializing *****"
Open "Words.dat" For Input As #1
Do Until EOF(1)
    CheckQueueSound
    A% = A% + 1
    Input #1, AA$
    Input #1, BB$
    Words(A%) = LTrim$(RTrim$(UCase$(AA$)))
    BB$ = LTrim$(RTrim$(BB$))
    Speech(A%) = BB$
    Print AA$, BB$
Loop
Close #1
'Print "Hello"
'SCREEN 0: WIDTH 80

'SLEEP 1
'say "PLEASE WAIT"
'SLEEP 1
'say "Hello this is a test"
'Print "Test"
'Do
'Loop Until Not SoundPlayingInChan(VoiceChannel)
'---
h& = LoadSound("TONOWH~2.MID")
PlaySoundNow h&
'say "Row Row Row Your boat"
'Do: Loop Until Not SoundPlayingInChan(VoiceChannel)
'say "gently down the stream"
'VoiceChannel = 2
'say "Row Row Row Your boat"
'Do: Loop Until Not SoundPlayingInChan(VoiceChannel)

' BUG: For some reason , once more is one time too many and the next line wont speak
say "                                                                   "
Do: Loop Until Not SoundPlayingInChan(VoiceChannel)

Print "Can you hear the calling"
say "Can you hear the calling"
Do: Loop Until Not SoundPlayingInChan(VoiceChannel)
'say "                                                                              "
'DO: LOOP UNTIL NOT SoundPlayingInChan(VoiceChannel)
Print "of the raving wind and water"
say "of the raving wind and water"
Do: Loop Until Not SoundPlayingInChan(VoiceChannel)
Print "We just keep dreaming of a land down the river"
say "We just keep dreaming    of a land down the river"
Do: Loop Until Not SoundPlayingInChan(VoiceChannel)
Print "We are always on our way to find a place we belong"
say "We are always on our way to find a place we belong"
Do: Loop Until Not SoundPlayingInChan(VoiceChannel)

Print "END"
End




Function Capital% (IsCap$)
    If IsCap$ = "" Then Exit Function
    If IsCap$ = UCase$(IsCap$) And IsCap$ <> LCase$(IsCap$) Then Capital = -1 Else Capital = 0
End Function




Sub GuessWord (WS$)
    NewWord WS$
End Sub


DefInt A-Z
Sub NewWord (w$)
    Shared VoiceChannel As Integer
    Shared SCHandle() As Long
    REENTER:
    Cls
    'PRINT "*** UNKNOWN WORD ***"
    Print "Input a list of sound codes for the word ''"; UCase$(w$); "''."
    'PRINT "The sound codes are:"
    Print "A  as in bAnanA:    a", , "A  as in mAp:       A"
    Print "A  as in bOther:    1", , "Ay as in dAY:       2"
    Print "A  as in bAd:       3", , "Aw as in nOW:       4"
    Print "Aw as in sAW:       5", , "B  as in BaBy:      b"
    Print "Ch as in CHin:      c", , "D  as in unDone:    d"
    Print "E  as in bEt:       e", , "Ea as in bEAt:      E"
    Print "F  as in FiFty:     f", , "G  as in Go:        g"
    Print "G  as in Gem:       G", , "H  as in Hat:       h"
    Print "I  as in tIp:       i", , "I  as in bUY:       I"
    Print "Ir as in bIRd:      j", , "K  as in CooK:      k"
    Print "L  as in pooL:      l", , "M  as in diM:       m"
    Print "N  as in No:        n", , "Ng as in siNG:      N"
    Print "O  as in bOne:      o", , "Oi as in cOIn:      O"
    Print "O  as in jOb:       6", , "Oo as in wOOd:      7"
    Print "P  as in PePPer:    p", , "R  as in RaRe:      r"
    Print "S  as in leSS:      s", , "Sh as in SHy:       S"
    Print "T  as in aTTack:    t", , "Th as in THen:      T"
    Print "Th as in THirteen:  H", , "U  as in rUle:      u"
    Print "U  as in hUmdrUm:   U", , "U  as in Union:     8"
    Print "U  as in cUrable:   9", , "V  as in giVe:      v"
    Print "W  as in We:        w", , "W  as in ONe:       W"
    Print "Z  as in raISE:     z", , "Pause (about 25ms): 0"


    Input "Sound codes? ", SC$

    If SC$ = "" Then End
    For B = 1 To Len(SC$)
        S$ = Mid$(SC$, B, 1)
        EnqueSound SCHandle(Asc(S$)), VoiceChannel
    Next
    Do: Loop Until Not SoundPlayingInChan(VoiceChannel)
    Print "Was that the way you say ''"; w$; "''?  (Y/N)";
    Input YN$
    If UCase$(YN$) = "N" Then GoTo REENTER
    If UCase$(YN$) = "Y" Then
        Print "Teaching the program a new word..."
        FFF = FreeFile
        Open "Words.dat" For Append As FFF
        Print #FFF, w$
        Print #FFF, SC$
        Close FFF
        Print "Re-loading word list..."
        fh% = FreeFile
        Open "Words.dat" For Input As #fh%
        Do Until EOF(fh%)
            A% = A% + 1
            Input #fh%, AA$
            Input #fh%, BB$
            Words(A%) = LTrim$(RTrim$(UCase$(AA$)))
            BB$ = LTrim$(RTrim$(BB$))
            Speech(A%) = BB$
        Loop
        Close fh%
    End If
    Print "DONE!"
End Sub


'------------------------------------------------------------------------------

Sub say (e$)
    Shared VoiceChannel
    If e$ = "" Then Exit Sub
    Ue$ = UCase$(e$)

    B = 1
    Do
        T$ = Mid$(Ue$, B, 1)
        If T$ <> " " Then
            sc$ = sc$ + T$
        Else
            If sc$ = "" Then 'T$ euqaled space twice so nothing got inserted
                EnqueSound SCHandle(Asc("0")), VoiceChannel
            Else
                'PRINT "Seeking"
                found% = Match(sc$, Words())
                If found% Then
                    'PRINT "Found"
                    For C = 1 To Len(Speech$(found%))
                        s$ = Mid$(Speech$(found%), C, 1)
                        EnqueSound SCHandle(Asc(s$)), VoiceChannel
                    Next C
                    EnqueSound SCHandle(Asc("0")), VoiceChannel
                Else
                    Do: Loop Until Not SoundPlayingInChan(VoiceChannel)
                    'PRINT "NewWord"
                    NewWord sc$
                End If
                sc$ = ""
            End If
        End If
        B = B + 1
        'PRINT "B "; B
    Loop Until B = Len(e$) + 1
    If sc$ = "" Then 'T$ euqaled space at end of line so nothing got inserted
        EnqueSound SCHandle(Asc("0")), VoiceChannel
    Else
        found% = Match(sc$, Words())
        If found% Then
            'PRINT "Found"
            For C = 1 To Len(Speech$(found%))
                s$ = Mid$(Speech$(found%), C, 1)
                EnqueSound SCHandle(Asc(s$)), VoiceChannel
            Next C
            EnqueSound SCHandle(Asc("0")), VoiceChannel
        Else
            Do: Loop Until Not SoundPlayingInChan(VoiceChannel)
            'PRINT "NewWord"
            NewWord sc$
        End If
    End If
End Sub

Function Match (s$, SA() As String)
    Dim A As Integer
    A = 1
    Do
        If s$ = SA(A) Then Match% = A: Exit Function
        A = A + 1
    Loop Until (A = UBound(SA) + 1) Or (SA(A) = "")
End Function



Function LoadSound& (Filename$)
    'PRINT Filename$: DO: LOOP UNTIL INKEY$ <> ""
    Dim SoundSet$
    'Not a static function, so shouldn't need to init SoundSet, but should we?
    'LoadSound=0, then
    'SELECT CASE UCASE$(RIGHT$(Filename$, 4))
    '  CASE ".WAV": SoundSet$ = "VOL,SYNC,LEN,PAUSE"
    '  CASE ".OGG": SoundSet$ = "VOL,SYNC,LEN,PAUSE"
    '  CASE ".AIF": SoundSet$ = "VOL,SYNC,LEN,PAUSE"
    '  CASE ".RIF": SoundSet$ = "VOL,SYNC,LEN,PAUSE"
    '  CASE ".VOC": SoundSet$ = "VOL,SYNC,LEN,PAUSE"
    '  CASE ".MID": SoundSet$ = "VOL"
    '  CASE ".MOD": SoundSet$ = "VOL,PAUSE"
    '  CASE ".MP3": SoundSet$ = "VOL,PAUSE,SETPOS"
    'END SELECT
    'IF SoundSet$ <> "" THEN
    LoadSound& = _SndOpen(Filename$) ', SoundSet$)
End Function

Sub SetMaxChan (NewMaxChan%)
    Shared SndChan() As String
    Shared MaxChan%

    MaxChan% = NewMaxChan%
    ReDim _Preserve SndChan(0 To MaxChan%) As String

End Sub

Sub CheckQueueSound
    Shared SndChan() As String
    Shared MaxChan%

    For I% = 0 To MaxChan%
        If SndChan(I%) <> "" Then
            CurrSndPlaying& = CVL(Left$(SndChan(I%), 4))
            If Not _SndPlaying(CurrSndPlaying&) Then
                _SndClose CurrSndPlaying&
                SndChan(I%) = Right$(SndChan(I%), Len(SndChan(I%)) - 4)
                If SndChan(I%) <> "" Then
                    CurrSndPlaying& = CVL(Left$(SndChan(I%), 4))
                    _SndPlay CurrSndPlaying&
                End If
            End If
        End If
    Next
End Sub

Sub PlaySoundNow (Handle&)
    'TempHandle& = _SNDCOPY(Handle&)
    _SndPlay Handle&
End Sub

Sub EnqueSound (Handle&, Chan%)
    If Handle& = 0 Then
        Print "Bad Handle Passed to EnqueSound:"
        Exit Sub
    End If
    NewHandle& = _SndCopy(Handle&)
    If SndChan(Chan%) = "" Then _SndPlay NewHandle&
    SndChan(Chan%) = SndChan(Chan%) + MKL$(NewHandle&)
End Sub

Function TABLE256% (Bytelist() As ByteArray, Index%)
    TABLE256 = Asc(Bytelist(Index%).Byte)
End Function

Function SoundPlayingInChan (Chan%)
    CheckQueueSound
    If SndChan(Chan%) <> "" Then SoundPlayingInChan = -1

End Function

