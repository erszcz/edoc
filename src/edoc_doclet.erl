%% =====================================================================
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @copyright 2003-2006 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Standard doclet module for EDoc.

%% Note that this is written so that it is *not* depending on edoc.hrl!

-module(edoc_doclet).

%% @headerfile "../include/edoc_doclet.hrl"
-include("../include/edoc_doclet.hrl").

-export_type([command/0,
	      context/0,
	      doclet_gen/0,
	      doclet_toc/0,
	      no_app/0]).

-type command() :: doclet_gen()
		 | doclet_toc().

%% @type context().
%%    Context for doclets.
-type context() :: #doclet_context{dir :: string(),
				   env :: edoc_lib:edoc_env(),
				   opts :: [term()]}.

%% @type no_app().
%%    A value used to mark absence of an Erlang application
%%    context. Use the macro `NO_APP' defined in
%%    <a href="edoc_doclet.hrl">`edoc_doclet.hrl'</a>
%%    to produce this value.
-type no_app() :: ?NO_APP.

%% @type doclet_gen().
%%    Doclet command.
-type doclet_gen() :: #doclet_gen{sources :: [string()],
				  app :: no_app() | atom(),
				  modules :: [module()]}.

%% @type doclet_toc().
%%    Doclet command.
-type doclet_toc() :: #doclet_toc{paths :: [string()],
				  indir :: string()}.

-callback run(command(), context()) -> ok.
