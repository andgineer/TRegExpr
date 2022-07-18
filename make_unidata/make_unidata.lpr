program make_unidata;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  fpwidestring,
  StrUtils;

const
  FILE_Input	 = 'UnicodeData.txt';
  FILE_Output	 = 'regexpr_unicodedata.pas';

  MODE_UpperLowerCase		 = 1;
  MODE_UnicodeGeneralCategory	 = 2;

  UGC_OtherNumber		 = 10;
  UnicodeCategoryNames: array[0..29] of String[2] = (
    'Lu', // UGC_UppercaseLetter
    'Ll', // UGC_LowercaseLetter
    'Lt', // UGC_TitlecaseLetter
    'Lm', // UGC_ModifierLetter
    'Lo', // UGC_OtherLetter
    'Mn', // UGC_NonSpacingMark
    'Mc', // UGC_CombiningMark
    'Me', // UGC_EnclosingMark
    'Nd', // UGC_DecimalNumber
    'Nl', // UGC_LetterNumber
    'No', // UGC_OtherNumber
    'Pc', // UGC_ConnectPunctuation
    'Pd', // UGC_DashPunctuation
    'Ps', // UGC_OpenPunctuation
    'Pe', // UGC_ClosePunctuation
    'Pi', // UGC_InitialPunctuation
    'Pf', // UGC_FinalPunctuation
    'Po', // UGC_OtherPunctuation
    'Sm', // UGC_MathSymbol
    'Sc', // UGC_CurrencySymbol
    'Sk', // UGC_ModifierSymbol
    'So', // UGC_OtherSymbol
    'Zs', // UGC_SpaceSeparator
    'Zl', // UGC_LineSeparator
    'Zp', // UGC_ParagraphSeparator
    'Cc', // UGC_Control
    'Cf', // UGC_Format
    'Cs', // UGC_Surrogate
    'Co', // UGC_PrivateUse
    'Cn' // UGC_Unassigned
  );
var
  slIn, // Input file: Unicode codepoint definitions
  slOut: TStringList; // Output file: Pascal unit
  dtIn: TDateTime; // Input file's modification time

  // Go through whole file, pick one part of each splitted line as per MODE_* for the mode.
  procedure EnumLines(iTargetColumn, iMode: Integer);
  var
    iLine: Integer; // Current line of input file
    aColumn: TStringArray; // Separating by semicolon per line
    sCategory, // Unicode category as text
    sTarget: String; // Output array element value: lowercase/uppercase/same as input codepoint
    iHex: LongInt; // Parse hexadecimal codepoint text
    iExpected: Cardinal; // Cannot be WORD because Unicode codepoints can be higher than U+FFFF
    bIsWordCharacter: Boolean;
    iCategory: Byte; // Unicode category as index of the array

    // Detect category index from text of the file's line (or gap inbetween).
    procedure CharCategory(sCategory: String; iCodepoint: Cardinal);
    var
      iCode: Integer; // Category index
    begin
      iCategory:= $FF; // Yet unknown category
      for iCode:= Low(UnicodeCategoryNames) to High(UnicodeCategoryNames) do
      begin
        if UnicodeCategoryNames[iCode] = sCategory then // Found
        begin
          iCategory:= iCode;
          break;
        end;
      end;

      if iCategory = $FF then
      begin
        slOut.Add(Format('// Error on line %d: unexpected category "%s" for codepoint U+%.4x!', [iLine, sCategory, iCodepoint]));
        exit;
      end;

      // Is this grapheme considered to be the character of a whole textual word?
      case iCodepoint of
        $005F: // Underscore
          bIsWordCharacter:= TRUE;
        $DC00.. $FFFF: // Low surrogate begin
          bIsWordCharacter:= FALSE;
      else
        bIsWordCharacter:= iCategory <= UGC_OtherNumber;
      end;
      if bIsWordCharacter then
        iCategory:= iCategory or $80; // Set 7th=last bit

      slOut.Add(Format('    %d%s // U+%.4x', [iCategory, IfThen(iCodepoint < $FFFF, ','), iCodepoint]));
    end;

  begin
    iExpected:= 0; // Start with codepoint U+0000
    for iLine:= 0 to slIn.Count do
    begin
      aColumn:= slIn[iLine].Split([';']);

      // Format: "0061;LATIN SMALL LETTER A;Ll;0;L;;;;;N;;;0041;;0041"
      // Columns: 0=codepoint, 2=category, 12=uppercase, 13=lowercase
      if Length(aColumn) <> 15 then
      begin
        slOut.Add(Format('// Error on line %d: not 15 columns, but instead %d!', [iLine, Length(aColumn)]));
        continue;
      end;

      // Not all codepoints are assigned, and the large range of U+4E00 to
      // U+9FFF are CJK Ideographs, not knowing letter casing anyway. Thus
      // the input file has gaps instead of one line per codepoint. Yet we
      // won't go beyond U+FFFF.
      iHex:= Hex2Dec(aColumn[0]);

      case iMode of
        MODE_UnicodeGeneralCategory:
          begin
            while (iHex > iExpected) and (iExpected <= $FFFF) do
            begin
              case iExpected of
                $3400..$4DBF, // CJK Ideograph Extension A
                $4E00..$9FFF, // CJK Ideograph
                $AC00..$D7A3: // Hangul Syllable
                  sCategory:= 'Lo';
                $D800..$DB7F, // Non Private Use High Surrogate
                $DB80..$DBFF, // Private Use High Surrogate
                $DC00..$DFFF: // Low Surrogate
                  sCategory:= 'Cs';
                $E000..$F8FF: // Private Use
                  sCategory:= 'Co';
              else // Codepoint is not in the input file at all
                sCategory:= 'Cn'; // Unassigned
              end;

              CharCategory(sCategory, iExpected);
              Inc(iExpected);
            end;

            // The actual grapheme, after filling the potential gap
            if iExpected <= $FFFF then
              CharCategory(aColumn[2], iExpected);
          end;

        MODE_UpperLowerCase:
          begin
            while (iHex > iExpected) and (iExpected <= $FFFF) do
            begin
              slOut.Add(Format('    #$%.4x%s // U+%.4x', [iExpected, IfThen(iExpected < $FFFF, ','), iExpected]));
              Inc(iExpected);
            end;

            // The actual grapheme, after filling the potential gap
            if iExpected <= $FFFF then
            begin
              // Is the column we want filled? Otherwise it just remains the same codepoint.
              sTarget:= aColumn[0];
              if aColumn[iTargetColumn] <> '' then
                if Length(aColumn[iTargetColumn]) <> 4 then // Only U+0000..U+FFFF is of our interest
                  slOut.Add(Format('// Error on line %d: expected a 4 digit referring codepoint U+%s, but got %d digits instead!', [iLine, aColumn[iTargetColumn], Length(aColumn[iTargetColumn])]))
                else
                  sTarget:= aColumn[iTargetColumn]; // Column 12=uppercase, 13=lowercase
              slOut.Add(Format('    #$%s%s // U+%s', [sTarget, IfThen(iExpected < $FFFF, ','), aColumn[0]]));
            end;
          end;

      else
        slOut.Add(Format('// Error: unexpected mode %d!', [iMode]));
        break;
      end;

      Inc(iExpected);
      if iExpected > $FFFF then // Ignore every input file codepoint above U+FFFF
        break;
    end;
    slOut.Add('  );');
  end;

begin
  slIn:= TStringList.Create;
  slIn.LoadFromFile(FILE_Input);
  dtIn:= FileDateToDateTime(FileAge(FILE_Input));

  slOut:= TStringList.create;
  slOut.Add('unit regexpr_unicodedata;');
  slOut.Add(Format('// This unit was generated on %s, %s.', [DateToStr(date()), TimeToStr(Time())]));
  slOut.Add(Format('// Source data file "%s" as of %s, %s,', [FILE_Input, DateToStr(dtIn), TimeToStr(dtIn)]));
  slOut.Add('//  downloaded from: https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt');
  slOut.Add('');
  slOut.Add('interface');
  slOut.Add('');
  slOut.Add('const');

  slOut.Add('  // bits 0..6: Category 0..29 from FPC''s unicodedata.pas');
  slOut.Add('  // bit     7: Is word character?');
  slOut.Add('  CharCategoryArray: packed array[0..$FFFF] of Byte = (');
  EnumLines(2, MODE_UnicodeGeneralCategory);

  slOut.Add('');
  slOut.Add('  // Get uppercase letter from any letter.');
  slOut.Add('  CharUpperArray: packed array[0..$FFFF] of WideChar = (');
  EnumLines(12, MODE_UpperLowerCase);

  slOut.Add('');
  slOut.Add('  // Get lowercase letter from any letter.');
  slOut.Add('  CharLowerArray: packed array[0..$FFFF] of WideChar = (');
  EnumLines(13, MODE_UpperLowerCase);

  slOut.Add('');
  slOut.Add('implementation');
  slOut.Add('');
  slOut.Add('end.');

  slOut.SaveToFile(FILE_Output);
  slOut.Free;

  slIn.Free;

end.
