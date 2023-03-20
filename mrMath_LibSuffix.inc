// ###################################################################
// #### This file is part of the mathematics library project, and is
// #### offered under the licence agreement described on
// #### http://www.mrsoft.org/
// ####
// #### Copyright:(c) 2023, Michael R. . All rights reserved.
// ####
// #### Unless required by applicable law or agreed to in writing, software
// #### distributed under the License is distributed on an "AS IS" BASIS,
// #### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// #### See the License for the specific language governing permissions and
// #### limitations under the License.
// ###################################################################
// ###################################################################
// #### contains all the possible LibSuffixes for different compiler versions (if suffix is possible)
// ###################################################################

{$DEFINE VER340ORUP}

{$IFDEF VER210}
{$LIBSUFFIX '210'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER220}
{$LIBSUFFIX '220'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER230}
{$LIBSUFFIX '230'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER240}
{$LIBSUFFIX '240'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER250}
{$LIBSUFFIX '250'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER260}
{$LIBSUFFIX '260'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER270}
{$LIBSUFFIX '270'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER280}
{$LIBSUFFIX '280'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER290}
{$LIBSUFFIX '290'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER300}
{$LIBSUFFIX '300'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER310}
{$LIBSUFFIX '320'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER320}
{$LIBSUFFIX '320'}
{$UNDEF VER340ORUP}
{$ENDIF}

{$IFDEF VER330}
{$LIBSUFFIX '330'}
{$UNDEF VER340ORUP}
{$ENDIF}

// for all others we try to use AUTO suffix
{$IFDEF VER340ORUP}
{$LIBSUFFIX AUTO}
{$ENDIF}