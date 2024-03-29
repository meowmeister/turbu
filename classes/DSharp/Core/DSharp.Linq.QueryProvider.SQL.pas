(*
  Copyright (c) 2011, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Linq.QueryProvider.SQL;

interface

uses
  DSharp.Linq.QueryProvider,
  DSharp.Core.Expressions;

type
  TSQLQueryProvider<T> = class(TQueryProvider<T>)
  public
    function Execute(Expression: IExpression): TObject; override;
  end;

  TExpressionHelper = class helper for TExpression
  public
    function ToSQL: string;
  end;

implementation

uses
//  DSharp.Core.Generics,
  SysUtils;

{ TExpressionHelper }

function TExpressionHelper.ToSQL: string;
begin
  if Self is TLogicalAndExpression then
  begin
    Result := '(' + TExpression(TEqualExpression(Self).Left).ToSQL + ' and ' +
      TExpression(TEqualExpression(Self).Right).ToSQL + ')'
  end
  else
  if Self is TLogicalOrExpression then
  begin
    Result := '(' + TExpression(TEqualExpression(Self).Left).ToSQL + ' or ' +
      TExpression(TEqualExpression(Self).Right).ToSQL + ')'
  end
  else
  if Self is TEqualExpression then
  begin
    Result := '(' + TExpression(TEqualExpression(Self).Left).ToSQL + ' = ' +
      TExpression(TEqualExpression(Self).Right).ToSQL + ')'
  end
  else
  if Self is TPropertyExpression then
  begin
    Result := '[' + TPropertyExpression(Self).Name + ']';
  end
  else
  if Self is TStringConstantExpression then
  begin
    Result := QuotedStr(TStringConstantExpression(Self).Value);
  end;
end;

{ TSQLQueryProvider }

function TSQLQueryProvider<T>.Execute(Expression: IExpression): TObject;
var
  s: string;
  x: T;
begin
  s := TExpression(Expression).ToSQL;

//  Result := TEnumerableEx<T>.Create([]);
end;


end.