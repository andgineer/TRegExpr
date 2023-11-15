{$ifdef windows}
  {$apptype console}
{$endif}

program test;

uses
  SysUtils,
  Classes,
  regexpr in '../src/regexpr.pas';

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
const BenchmarkPatterns:array[0..37] of ansistring=('Twain',
                                                   '(?i)Twain',
                                                   '[a-z]shing',
                                                   'Huck[a-zA-Z]+|Saw[a-zA-Z]+',
                                                   '.',   // Benchmark invocation time.
                                                   '(.)',
                                                   'e',
                                                   '(e)',
                                                   '(?s).{1,45} ',
                                                   '(?s)\G.{1,45} ',
                                                   '\G(?s).{1,45} ',
                                                   '(?s).{1,45}? ',
                                                   '(?s)\G.{1,45}? ',
                                                   '\G(?s).{1,45}? ',
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
                                                   '["''][^"'']{0,30}[?!\.]["'']',
                                                   'Tom(.{3,3}|.{5,5})*Finn',
                                                   'Tom(...|.....)*Finn',
                                                   'Tom(...|.....)*?Finn',
                                                   'Tom((...|.....){2,9}\s){1,5}?Finn',
                                                   'Tom((...|.....){2,9}?\s){1,5}Finn',
                                                   '\G(?is).(?=.*$)',
                                                   '\G(?is).(?=(.){1,5}?$)?',
                                                   '\G(?is).(?=.*+$)',
                                                   '\G(?is).{10,10}(?=(e|y|on|fin|.){0,20})',
                                                   '\G(?is).{10,10}(?=(?>e|y|on|fin|.){0,20})',
                                                   'Tom(?!.*Finn)',
                                                   '(?i)(?>[aeoui][bcdfghjklmnpqrstvwxyz \r\n]*){1,40}',
                                                   '(?i)[ bdlm][abdegij][mnoprst]'
                                                   );


var i,j, r, RCount:integer;
    stext,expr:ansistring;
    sl:TFileStream;
    t1,t2,t_all:longword;
    Regex:TRegExpr;
begin
 RCount := 1;
 for i := 0 to argc - 2 do begin
   if argv[i] = '-c' then begin
     RCount := StrToIntDef(argv[i+1], 1);
     if RCount > 200 then RCount := 200;
     break;
   end;
 end;
 try
  sl:=TFileStream.Create('mtent12.txt',fmOpenRead);
  try
   SetLength(stext,sl.Size);
   sl.Read(stext[1],sl.Size);
  finally
   sl.Free;
  end;

  writeln('Running ', RCount, ' repeats for each benchmark. Use -c <n> to change');
  writeln;
  writeln(' ':50,'      Time     | Match count');
  writeln('==============================================================================');
  writeln('regexpr.pas:');

  t_all := 0;
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
    Regex.ModifierS:=false;
    Regex.Expression:=expr;
    Regex.Compile;
    Regex.SlowChecksSizeMax:=0;

    try
     write('/'+BenchmarkPatterns[i]+'/ : ':60);
     t1:=GetTickCount;
     for r := 1 to RCount do begin
       j:=0;
       if Regex.Exec(stext) then begin
        repeat
         inc(j);
        until not Regex.ExecNext;
       end;
     end;
     t2:=GetTickCount;
     writeln(((t2-t1) div RCount):11,' ms |',j:12);
     t_all := t_all + (t2-t1);
    finally
     Regex.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;{}
 writeln('Total:', '':54, (t_all div RCount):11,' ms |');
 finally
  stext:='';
 end;
end.

