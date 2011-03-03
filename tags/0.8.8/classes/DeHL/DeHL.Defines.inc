(*
* Copyright (c) 2009, Ciobanu Alexandru
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

{$IF CompilerVersion < 21}
  DEHL requires at least Delphi 2009 to build and run
{$IFEND}

{ Common to all Delphi 21+ releases }
{$DEFINE BUG_GENERIC_INCOMPAT_TYPES}
{$DEFINE BUG_URW1100}
{$DEFINE BUG_IS_OP_CLASS}
{$DEFINE BUG_RTTI_ELEMENTTYPE}


{ Architecture checking }
{$IF SizeOf(NativeInt) = SizeOf(LongInt)}
  {$DEFINE ARCH_32_BIT}
{$ELSEIF SizeOf(NativeInt) = SizeOf(Int64)}
  {$DEFINE ARCH_64_BIT}
{$ELSE}
  This architecture is not supported!
{$IFEND}

{ Optimizations }
{$DEFINE OPTIMIZED_SORT}
