{$ifdef windows}
  {$apptype console}
{$endif}

program test;

uses
  SysUtils,
  Classes,
  RegExpr in '../src/RegExpr.pas';

// benchmark from http://lh3lh3.users.sourceforge.net/reb.shtml
{
const BenchmarkPatterns2:array[0..4] of ansistring=('installation',
                                                    '(?:[a-zA-Z][a-zA-Z0-9]*)://(?:[^ /]+)(?:/[^ ]*)?',
                                                    '(?:[^ @]+)@(?:[^ @]+)',
                                                    '(?:[0-9][0-9]?)/(?:[0-9][0-9]?)/(?:[0-9][0-9](?:[0-9][0-9])?)',
                                                    '(?:[a-zA-Z][a-zA-Z0-9]*)://(?:[^ /]+)(?:/[^ ]*)?|(?:[^ @]+)@(?:[^ @]+)');

const BenchmarkPatterns:array[0..4] of ansistring=('Twain',
                                                   'Huck[a-zA-Z]+',
                                                   '[a-zA-Z]+ing',
                                                   'Tom|Sawyer|Huckleberry|Finn',
                                                   'Tom.{0,30}river|river.{0,30}Tom');

const BenchmarkPatterns:array[0..4] of ansistring=('installation',
                                                   '([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/[^ ]*)?',
                                                   '([^ @]+)@([^ @]+)',
                                                   '([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)',
                                                   '([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/[^ ]*)?|([^ @]+)@([^ @]+)');
}
const BenchmarkPatterns:array[0..14] of ansistring=('Twain',
                                                   '(?i)Twain',
                                                   '[a-z]shing',
                                                   'Huck[a-zA-Z]+|Saw[a-zA-Z]+',
                                                   '\b\w+nn\b',
                                                   '[a-q][^u-z]{13}x',
                                                   'Tom|Sawyer|Huckleberry|Finn',
                                                   '(?i)Tom|Sawyer|Huckleberry|Finn',
                                                   '.{0,2}(Tom|Sawyer|Huckleberry|Finn)',
                                                   '.{2,4}(Tom|Sawyer|Huckleberry|Finn)',
                                                   'Tom.{10,25}river|river.{10,25}Tom',
                                                   '[a-zA-Z]+ing',
//                                                 '[a-zA-Z]{0,12}ing',
                                                   '\s[a-zA-Z]{0,12}ing\s',
                                                   '([A-Za-z]awyer|[A-Za-z]inn)\s',
                                                   '["''][^"'']{0,30}[?!\.]["'']');


var i,j:integer;
    stext,expr:ansistring;
    sl:TFileStream;
    t1,t2:longword;
    Regex:TRegExpr;
begin
 try
  sl:=TFileStream.Create('mtent12.txt',fmOpenRead);
  try
   SetLength(stext,sl.Size);
   sl.Read(stext[1],sl.Size);
  finally
   sl.Free;
  end;

  writeln(' ':50,'      Time     | Match count');
  writeln('==============================================================================');
  writeln('RegExpr.pas:');

  for i:=low(BenchmarkPatterns) to high(BenchmarkPatterns) do begin
   try
    expr:=BenchmarkPatterns[i];
    if pos('(?i)',expr)=1 then begin
     Delete(expr,1,4);
     j:=1;
    end else begin
     j:=0;
    end;
    Regex:=TRegExpr.Create;
    Regex.ModifierI:=j<>0;
    Regex.Expression:=expr;
    Regex.Compile;
    try
     write('/'+BenchmarkPatterns[i]+'/ : ':50);
     t1:=GetTickCount;
     j:=0;
     if Regex.Exec(stext) then begin
      repeat
       inc(j);
      until not Regex.ExecNext;
     end;
     t2:=GetTickCount;
     writeln(t2-t1:11,' ms |',j:12);
    finally
     Regex.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;{}
 finally
  stext:='';
 end;
 readln;
end.

