// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2011, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################
// ###################################################################
// #### certain MACOS contributions and testing William Cantrall
// ###################################################################

//  wrc dispatch_get_global_queue
// dispatch_group_create and dispatch_group_async_f and  dispatch_group_wait
unit GCDDispatch;

interface

{$IFDEF DARWIN}
{$DEFINE MACOS}
{$ENDIF}

{$IFDEF MACOS}

uses
{$IFDEF FPC}
  sysutils;
{$ELSE}
  system.sysutils;
{$ENDIF}

type
  dispatch_object_t = IntPtr;
  dispatch_queue_t = dispatch_object_t;
  dispatch_group_t = dispatch_object_t;
  dispatch_time_t = UInt64;
  size_t = Integer;
  dispatch_function_t =  procedure(context:  pointer)cdecl;
  //dispatch_block_t  = TProc;
const
  libdispatch = '/usr/lib/system/libdispatch.dylib';
  DISPATCH_QUEUE_SERIAL = nil;
  DISPATCH_QUEUE_PRIORITY_DEFAULT = 0;
  DISPATCH_TIME_FOREVER = UInt64($8000000000000000);  //????

{$IF NOT DECLARED(_PU)}
const
  {$IFDEF UNDERSCOREIMPORTNAME}
  _PU = '_';
  {$ELSE}
  _PU = '';
  {$ENDIF}
  {$EXTERNALSYM _PU}
{$IFEND}

// dispatch_group_t dispatch_group_create(
//   void);
function dispatch_group_create():dispatch_group_t;cdecl; external libdispatch name _PU + 'dispatch_group_create';
(*
 long dispatch_group_wait(
   dispatch_group_t group,
   dispatch_time_t timeout);
   *)
function dispatch_group_wait(group: dispatch_group_t;timeout: dispatch_time_t): UInt64;cdecl;external libdispatch name _PU + 'dispatch_group_wait';
//procedure dispatch_release(obj: dispatch_object_t); cdecl; external libdispatch name _PU + 'dispatch_release';
(*
void dispatch_async_f(
   dispatch_queue_t queue,
   void *context,
   dispatch_function_t work);
   *)
     //dispatch_get_global_queue(long priority, unsigned long flags);
function dispatch_get_global_queue(priority: LongInt;flags: LongInt): dispatch_queue_t;cdecl; external libdispatch name _PU + 'dispatch_get_global_queue';

(*
void dispatch_group_async_f(
   dispatch_group_t group,
   dispatch_queue_t queue,
   void *context,
   dispatch_function_t work);
*)
procedure dispatch_group_async_f(group: dispatch_group_t;queue:dispatch_queue_t;context:  pointer;work: dispatch_function_t);cdecl; external libdispatch name _PU + 'dispatch_group_async_f';
procedure dispatch_release(obj: dispatch_object_t); cdecl; external libdispatch name _PU + 'dispatch_release';

{$ENDIF}

implementation

end.
