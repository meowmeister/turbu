unit conversion_report;
{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

interface
uses
   sysUtils;
type
   IConversionReport = interface
   ['{FEA66171-DC2D-4CFE-874B-DF3F2A8F0932}']
      procedure setTasks(const value: integer);
      procedure setCurrentTask(const name: string; const steps: integer); overload;
      procedure setCurrentTask(const name: string); overload;
      procedure newStep(name: string);
      procedure makeHint(text: string; group: integer = -1);
      procedure makeNotice(text: string; group: integer = -1);
      procedure makeError(text: string; group: integer = -1);
      procedure fatal(errorMessage: string); overload;
      procedure fatal(error: Exception); overload;
      procedure PauseSteps;
      procedure ResumeSteps;
      procedure makeReport;
      property tasks: integer write setTasks;
   end;

implementation

end.
