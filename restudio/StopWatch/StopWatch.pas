{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$OPTIMIZATION ON}
{$IFDEF VER150}
  {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
  {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
  {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
{$ENDIF}
unit StopWatch;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
  StopWatch
   helper routines for precise time metering
   !! processor depended (works only with
      Intel Pentium and higher
   !! uses Int64 type (incompatible with old
      versions of Delphi)

  (c) 2002 Andrey V. Sorokin
  https://sorokin.engineer/
  andrey@sorokin.engineer

---------------------------------------------------------------
     Legal issues
---------------------------------------------------------------
 This software is provided as it is, without any kind of
 warranty given. Use it at your own risk.

 You may use this software ONLY in development of FREEWARE
 products, under the following restrictions :
 1. The origin of this software may not be mispresented, you must
    not claim that you wrote the original software. If you use
    this software in any kind of product, it would be appreciated
    that there in a information box, or in the documentation would
    be an acknowledgmnent like this
           Partial Copyright (c) 2000 by Andrey V. Sorokin
 2. You may not have any income from distributing this source
    to other developers. When you use this product in a comercial
    package, the source may not be charged seperatly.

 For usage in commertial development please contact author.
}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Classes;

type
 TCycles = Int64;
 TCPUkHz = Int64;
 TTimeInterval = Int64;

 TStopWatch = class
   private

    {$IFDEF FPC}
    fStartCycle : TDateTime;
    fCyclesElapsed : int64;
    {$ELSE}
    fStartCycle : TCycles;
    fCyclesElapsed : TCycles;
    {$ENDIF}
    function GetMicroSecs : TTimeInterval;
    function GetMilliSecs : TTimeInterval;
    function GetTimeStr : string;

   public
    constructor Create;
    procedure Start (AClear : boolean = True);
    procedure Stop;
    // Start/stop the timer
    // If AClear = True then previously accumulated
    // time will be cleared.

    class procedure Calibrate;
    // performes calibration of Start/Stop calls
    // (result stored in var StartStopDelayCycles)
    // You don't have to call this method because
    // it runs automatically (see initialization
    // section of this unit).

    property Cycles : TCycles read fCyclesElapsed;
    property MicroSecs : TTimeInterval read GetMicroSecs;
    property MilliSecs : TTimeInterval read GetMilliSecs;
    property TimeStr : string read GetTimeStr;
    // This methods return time estimated in different
    // units: CPU cycles, microseconds, milliseconds
    // and formatted in text string.

  end;

var
 SavedCPUClockKHz : TCPUkHz;
 // CPU clock frequency (in kHz) currently used for
 // reculculation of "ticks" into MSecs.
 // If 0 then first call to TStopWatch.MSec will assign
 // value from CalculateCPUClockKHz.
 // You can (and should) give to user ability to adjust it
 // to real value (my GetCPUClockKHz algorithm
 // is not very accurate :( )

function CPUClockKHz : TCPUkHz;
// Returns SavedCPUClockKHz value or, if it is 0
// first call CalculateCPUClockKHz and store
// result into SavedCPUClockKHz.
// !! For "real programmers": kHz = 1000 * Hz, not 1024 ;)))

function CalculateCPUClockKHz (ACalibrateTries : integer = 5) : TCPUkHz;
// Calibrate CPU speed, returns clock frequency in kHz
// ACalibrateTries - number of calibration tries
//  (the bigger gives better accuracy)
// Returns 0 if system is currently too buzy for precize operation
//  or -1 if no hardware for CPU frequency calculation is detected

function SystemClockStep : DWORD;
// Returns min.step of API function GetTickCount, in ms.

implementation

uses
 SysUtils,
 {$IFDEF FPC} DateUtils, {$ENDIF}
 MMSystem;

var
 StartStopDelayCycles : TCycles = 0;

type
 TCPUCycles = Int64; // MUST be 64-bits structure - used for storing RDTSC result

function GetCPUCycle: TCPUCycles;
// Returns current processor cycle number
// !! only for Intel Pentium and higher
 asm
  {$IFDEF FPC}
  rdtsc
  {$ELSE}
  DB $0F,$31 // rdtsc
  {$ENDIF}
 end; { of function GetCPUCycle
--------------------------------------------------------------}

function CalculateSystemClockStep : DWORD;
 // Returns minimal step of GetTickCount function
 var
  hThread : THandle;
  hProcess : THandle;
  SavedPriorityClass : DWORD;
  SavedPriority : DWORD;
  i : integer;
  t, t0 : DWORD;
 begin
  // GetSystemTimeAdjustment() (lpTimeIncrement / 100000) - only WinNT
  hProcess := GetCurrentProcess ();
  hThread := GetCurrentThread;
  SavedPriorityClass := GetPriorityClass (hProcess);
  SavedPriority := GetThreadPriority (hThread);
  try
    if SavedPriorityClass <> 0
     then SetPriorityClass (hProcess, REALTIME_PRIORITY_CLASS);
    if SavedPriority <> THREAD_PRIORITY_ERROR_RETURN
     then SetThreadPriority (hThread, THREAD_PRIORITY_TIME_CRITICAL);
    Result := 100; // initialize with big value
    Sleep (0); // we are trying to start from time slice start
    for i := 1 to 3 do begin
      t0 := GetTickCount;
      REPEAT
       t := GetTickCount;
      UNTIL t0 <> t;
      if t - t0 < Result
       then Result := t - t0;
     end;
    if Result = 0 // ?!!
     then Result := 1;
    finally begin
      if SavedPriorityClass <> 0
       then SetPriorityClass (hProcess, SavedPriorityClass);
      if SavedPriority <> THREAD_PRIORITY_ERROR_RETURN
       then SetThreadPriority (hThread, SavedPriority);
     end;
   end;
 end; { of function CalculateSystemClockStep
--------------------------------------------------------------}

var
 SavedSystemClockStep : DWORD = 0;
function SystemClockStep : DWORD;
 begin
  if SavedSystemClockStep <= 0
   then SavedSystemClockStep := CalculateSystemClockStep;
  Result := SavedSystemClockStep
 end; { of function GetSystemClockStep
--------------------------------------------------------------}

function CPUClockKHz;
 begin
  if SavedCPUClockKHz <= 0
   then SavedCPUClockKHz := CalculateCPUClockKHz;
  Result := SavedCPUClockKHz;
 end; { of function CPUClockKHz
--------------------------------------------------------------}

function CalculateCPUClockKHz;
 const
  MaxTimeStampTries = 10; // if we cannot make precise time stamp
  // more then MaxTimeStampTries times, then GetTimeStamps fails

 var
  ElapsedTicks : TCycles;
  ElapsedMs : DWORD;
  md, md0 : TCycles;

  pt, pt0 : TCycles;
  t0, t : DWORD;
  TickStep : DWORD;
  hThread : THandle;
  hProcess : THandle;
  SavedPriorityClass : DWORD;
  SavedPriority : DWORD;
  n : integer;
//  CalibrateTicks : Cardinal;
  TryNo : integer;
  Res : Int64;

 function GetTimeStamps (var ASystemTime : DWORD; var ACPUTick : TCycles; var ADelay : TCycles) : boolean;
 // Store into ASystemTime and ACPUTick current time stamps for
 // system and CPU, trying to do it accurate as much possible
 // Returns False if system too buzy for precize operations
  var
   tZ : DWORD;
  begin
   try
     if SavedPriorityClass <> 0
      then SetPriorityClass (hProcess, REALTIME_PRIORITY_CLASS);
     if SavedPriority <> THREAD_PRIORITY_ERROR_RETURN
      then SetThreadPriority (hThread, THREAD_PRIORITY_TIME_CRITICAL);
     Sleep (0); // we are trying to start from time slice start
     pt := GetCPUCycle;
     tZ := GetTickCount { *Converted from TimeGetTime* }div TickStep * TickStep + TickStep;
     REPEAT
      ACPUTick := GetCPUCycle;
      ASystemTime := GetTickCount; { *Converted from TimeGetTime* }
      if ASystemTime >= tZ
       then BREAK;
      pt := ACPUTick;
     UNTIL False;
     Result := ASystemTime = tZ;
     ADelay := GetCPUCycle - pt;
     finally begin
       if SavedPriorityClass <> 0
        then SetPriorityClass (hProcess, SavedPriorityClass);
       if SavedPriority <> THREAD_PRIORITY_ERROR_RETURN
        then SetThreadPriority (hThread, SavedPriority);
      end;
    end;
  end; { of function GetTimeStamps
..............................................................}
 begin
  Result := 0;

  TickStep := SystemClockStep;

  // Adjust ACalibrateTime
{
  CalibrateTicks := ACalibrateTime div TickStep;
  if ACalibrateTime mod TickStep <> 0
   then inc (CalibrateTicks);
  if CalibrateTicks < 5
   then CalibrateTicks := 5;
  ACalibrateTime := CalibrateTicks * TickStep;
}

  hProcess := GetCurrentProcess ();
  hThread := GetCurrentThread;
  SavedPriorityClass := GetPriorityClass (hProcess);
  SavedPriority := GetThreadPriority (hThread);

  for TryNo := 0 to ACalibrateTries do begin

  if timeBeginPeriod (1) = TIMERR_NOERROR
   then ; // if more accurate timer setting fails just use what we have

  try // to ensure we'll restore timer old accuracy

    n := 0;
    while n < MaxTimeStampTries do
     if not GetTimeStamps (t0, pt0, md0)
      then inc (n)
      else BREAK;
    if n >= MaxTimeStampTries
     then EXIT;
//    Sleep (ACalibrateTime); // Do not forget to uncomment ACalibrateTime adjustment
    Sleep (TickStep); // I think it's more accurate %-|
    n := 0;
    while n < MaxTimeStampTries do
     if not GetTimeStamps (t, pt, md)
      then inc (n)
      else BREAK;
    if n >= MaxTimeStampTries
     then EXIT;

    ElapsedTicks := pt - pt0;
    ElapsedMs := t - t0;

    finally timeEndPeriod (1); // restore timer old accurace
   end;

  if (md + md0) * 10 <= ElapsedTicks then begin
    // Error not too big - we can use this result
    Res := Round (1.0 * ElapsedTicks / ElapsedMs);
    if Res > Result
     then Result := Res;
   end;

  end; // of for TryNo

 Result := Round (Result * 0.997);
 // Ugly practice to use 'magic consts'..
 // but it gives better estimation - I don't know why :(

 end; { of function CalculateCPUClockKHz
--------------------------------------------------------------}


{==================== TStopWatch =============================}

constructor TStopWatch.Create;
 begin
  inherited;
  fCyclesElapsed := 0;
  fStartCycle := 0;
 end; { of constructor TStopWatch.Create
--------------------------------------------------------------}

function TStopWatch.GetMicroSecs;
 begin
  {$IFDEF FPC}
  Result := fCyclesElapsed * 1000;
  {$ELSE}
  Result := fCyclesElapsed * 1000 div CPUClockKHz;
  {$ENDIF}
 end; { of function TStopWatch.GetMicroSecs
--------------------------------------------------------------}

function TStopWatch.GetMilliSecs;
 begin
  {$IFDEF FPC}
  Result := fCyclesElapsed;
  {$ELSE}
  Result := fCyclesElapsed div CPUClockKHz;
  {$ENDIF}
 end; { of function TStopWatch.GetMilliSecs
--------------------------------------------------------------}

function TStopWatch.GetTimeStr : string;
 var
  t : TTimeInterval;
 begin
  t := MilliSecs;
  if t < 5
   then Result := Format ('%d mks', [MicroSecs])
  else if t < 5000
   then Result := Format ('%d ms', [t])
  else begin
    Result := Format ('%d', [(t div 1000) mod 60]);
    t := t div 1000 div 60;
    if t > 0 then begin
      Result := Format ('%d:', [(t div 1000) mod 60]) + Result;
      t := t div 1000 div 60;
      if t > 0 then
       Result := Format ('%d:', [(t div 1000) mod 60]) + Result;
     end
     else Result := Result + ' sec';
   end;
 end; { of function TStopWatch.GetTimeStr
--------------------------------------------------------------}

procedure TStopWatch.Start;
 begin
  if AClear
   then fCyclesElapsed := 0;
  {$IFDEF FPC}
  fStartCycle := Now;
  {$ELSE}
  fStartCycle := GetCPUCycle;
  {$ENDIF}
 end; { of procedure TStopWatch.Start
--------------------------------------------------------------}

procedure TStopWatch.Stop;
 begin
  {$IFDEF FPC}
  fCyclesElapsed := fCyclesElapsed + MilliSecondsBetween(Now, fStartCycle);
  {$ELSE}
  fCyclesElapsed := fCyclesElapsed +
   (GetCPUCycle - fStartCycle) - StartStopDelayCycles;
  {$ENDIF}
 end; { of procedure TStopWatch.Stop
--------------------------------------------------------------}

class procedure TStopWatch.Calibrate;
 var
  i : integer;
  Delay : TCycles;
 begin
  StartStopDelayCycles := 0;
  Delay := MaxInt;
  with TStopWatch.Create do try
     for i := 1 to 5 do begin
       Start (True);
       Stop;
       if Cycles < Delay
        then Delay := Cycles;
      end;
    finally Free;
   end;
  StartStopDelayCycles := Delay;
 end; { of class procedure TStopWatch.Calibrate
--------------------------------------------------------------}

initialization
 {$IFnDEF FPC}
 TStopWatch.Calibrate;
 {$ENDIF}

end.

