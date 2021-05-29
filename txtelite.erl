%%% ---------------------------------------------------------------------
%%% File: txtelite.erl
%%%
%%% @copyright Ian Bell & David Braben
%%% @author Richard Carlsson <richardc@it.uu.se>
%%% @version 1.5
%%%
%%% @doc Erlang implementation of the Elite universe, directly based on
%%% Ian Bell's txtelite.c 1.4, which is a C translation from the
%%% original 6502 source code used for the BBC Micro version of
%%% Acornsoft Elite. The behaviour of this code has been checked against
%%% the C version.
%%%
%%% Comments in this code are mostly copied straight from txtelite.c.

-module(txtelite).

-export([main/0]).

%%% The nature of basic mechanisms used to generate the Elite
%%% socio-economic universe are now widely known. A competent games
%%% programmer should be able to produce equivalent functionality. A
%%% competent hacker should be able to lift the exact system from the
%%% object code base of official conversions.
%%%
%%% This file may be regarded as defining the Classic Elite universe.
%%%
%%% It contains a C implementation of the precise 6502 algorithms used
%%% in the original BBC Micro version of Acornsoft Elite together with a
%%% parsed textual command testbed.
%%%
%%% Note that this is not the universe of David Braben's 'Frontier'
%%% series.
%%%
%%%
%%% ICGB 13/10/99
%%% iancgbell@email.com
%%% www.ibell.co.uk


%% ========================================================================
%% Game section

%% Define for NES-sanitised trade goods
%%-define(POLITICALLY_CORRECT, true).

-define(nocomms, 14).
-define(tonnes, 0).

-define(galsize, 256).
-define(AlienItems, 16).
-define(lasttrade, ?AlienItems).

-define(numforLave, 7).       %% Lave is 7th generated planet in galaxy one
-define(numforZaonce, 129).
-define(numforDiso, 147).
-define(numforRied, 46).

-define(fuelcost, 2).  %% 0.2 CR/Light year
-define(maxfuel, 70).  %% 7.0 LY tank

-define(base0, 16#5A4A).
-define(base1, 16#0248).
-define(base2, 16#B753).  %% Base seed for galaxy 1

%% plansys C struct:
%%  uint x, y;         // One byte unsigned
%%  uint economy       // These two are actually only 0-7
%%  uint govtype
%%  uint techlev       // 0-16 i think
%%  uint population    // One byte
%%  uint productivity  // Two byte
%%  uint radius        // Two byte (not used by game at all)
%%  uint human_colony; // boolean
%%  uint species_type;
%%  uint species_adj1;
%%  uint species_adj2;
%%  uint species_adj3;
%%  fastseedtype goatsoupseed
%%  char name[12]

-record(plansys,
	{x, y, economy, govtype, techlev, population,
	 productivity, radius, human_colony, species_type,
	 species_adj1, species_adj2, species_adj3,
	 goatsoupseed, name}).

%% Player workspace
%%
%% cash
%% fuel
%% holdspace
%% galaxynum       %% Galaxy number (1-8)
%% currentplanet   %% Current planet
%% shipshold       %% Contents of cargo bay
%% localmarket
%%
%% Should not be saved
%% galaxy          %% current galaxy (galsize-tuple of plansys records)

-record(game, {cash, fuel, holdspace, shipshold,
	       galaxynum, currentplanet, localmarket,
	       galaxy}).

govnames() ->
    {<<"Anarchy">>, <<"Feudal">>, <<"Multi-government">>,
     <<"Dictatorship">>, <<"Communist">>, <<"Confederacy">>,
     <<"Democracy">>, <<"Corporate State">>}.

get_govname(I) -> index(I, govnames()).

econnames() ->
    {<<"Rich Industrial">>, <<"Average Industrial">>,
     <<"Poor Industrial">>, <<"Mainly Industrial">>,
     <<"Mainly Agricultural">>, <<"Rich Agricultural">>,
     <<"Average Agricultural">>, <<"Poor Agricultural">>}.

get_econname(I) -> index(I, econnames()).

species_statures() -> { <<"Large">>, <<"Fierce">>, <<"Small">> }.

get_species_stature(I) -> index(I, species_statures()).

species_colorations() ->
    { <<"Green">>, <<"Red">>, <<"Yellow">>,
      <<"Blue">>, <<"Black">>, <<"Harmless">> }.

get_species_coloration(I) -> index(I, species_colorations()).

species_characteristics() ->
    { <<"Slimy">>, <<"Bug-Eyed">>, <<"Horned">>,
      <<"Bony">>, <<"Fat">>, <<"Furry">> }.

get_species_characteristic(I) -> index(I, species_characteristics()).

species_base_types() ->
    { <<"Rodents">>, <<"Frogs">>, <<"Lizards">>, <<"Lobsters">>,
      <<"Birds">>, <<"Humanoids">>, <<"Felines">>, <<"Insects">> }.

get_species_base_type(I) -> index(I, species_base_types()).

unitnames() -> {<<"t">>, <<"kg">>, <<"g">>}.

get_unitname(I) -> index(I, unitnames()).

%% tradegood C struct:
%%                    // In 6502 version these were:
%%  uint  baseprice   // one byte
%%  int16 gradient    // five bits plus sign
%%  uint  basequant   // one byte
%%  uint  maskbyte    // one byte
%%  uint  unit        // two bits
%%  char  name[20]    // longest="Radioactives"

%%  Commodity tuple for DB's price/availability generation system:
-record(comm, {baseprice, gradient, basequant, maskbyte, unit, name}).

-ifdef(POLITICALLY_CORRECT).
-define(SLAVES, <<"Robot Slaves">>).
-define(DRINK,  <<"Beverages">>).
-define(DRUGS,  <<"Rare Species">>).
-else.
-define(SLAVES,	<<"Slaves">>).
-define(DRINK,  <<"Liquor/Wines">>).
-define(DRUGS,  <<"Narcotics">>).
-endif.

commodities() ->
    {{comm, 16#13, -16#02, 16#06, 16#01, 0, <<"Food">>},
     {comm, 16#14, -16#01, 16#0A, 16#03, 0, <<"Textiles">>},
     {comm, 16#41, -16#03, 16#02, 16#07, 0, <<"Radioactives">>},
     {comm, 16#28, -16#05, 16#E2, 16#1F, 0, ?SLAVES},
     {comm, 16#53, -16#05, 16#FB, 16#0F, 0, ?DRINK},
     {comm, 16#C4, +16#08, 16#36, 16#03, 0, <<"Luxuries">>},
     {comm, 16#EB, +16#1D, 16#08, 16#78, 0, ?DRUGS},
     {comm, 16#9A, +16#0E, 16#38, 16#03, 0, <<"Computers">>},
     {comm, 16#75, +16#06, 16#28, 16#07, 0, <<"Machinery">>},
     {comm, 16#4E, +16#01, 16#11, 16#1F, 0, <<"Alloys">>},
     {comm, 16#7C, +16#0d, 16#1D, 16#07, 0, <<"Firearms">>},
     {comm, 16#B0, -16#09, 16#DC, 16#3F, 0, <<"Furs">>},
     {comm, 16#20, -16#01, 16#35, 16#03, 0, <<"Minerals">>},
     {comm, 16#61, -16#01, 16#42, 16#07, 1, <<"Gold">>},
     {comm, 16#AB, -16#02, 16#37, 16#1F, 1, <<"Platinum">>},
     {comm, 16#2D, -16#01, 16#FA, 16#0F, 2, <<"Gem-Stones">>},
     {comm, 16#35, +16#0F, 16#C0, 16#07, 0, <<"Alien Items">>}
    }.

get_commodity(I) -> index(I, commodities()).
   
tradnames() ->
    list_to_tuple([binary_to_list((get_commodity(I))#comm.name)
		   || I <- lists:seq(0, ?lasttrade)]).

init_shipshold(St) ->
    St#game{shipshold = erlang:make_tuple(?lasttrade+1, 0)}.

get_shipshold(I, St) ->
    index(I, St#game.shipshold).

update_shipshold(I, Delta, St) ->
    Now = get_shipshold(I, St),
    St#game{shipshold = update(I, St#game.shipshold, Now + Delta)}.

%% Attempt to buy Wants tonnes of fuel
gamefuel(Wants, St) ->
    Cash = St#game.cash,
    Has = St#game.fuel,
    Needs = if (Wants + Has) > ?maxfuel -> ?maxfuel - Has;
	       true -> Wants
	    end,
    Bought = if ?fuelcost > 0, (Needs * ?fuelcost) > Cash ->
		     Cash div ?fuelcost;
		true ->
		     Needs
	     end,
    St1 = St#game{fuel = Has + Bought,
		  cash = Cash - (?fuelcost * Bought)},
    {Bought, St1}.

%% markettype C struct:
%%  uint  quantity[lasttrade+1]
%%  uint  price[lasttrade+1]

-record(market, {quantities, prices}).

%% Functions for stock market

gamebuy(I, A, St) ->
    %% Try to buy amount A of good I.  Return amount bought.
    %% Cannot buy more than is available, can afford, or will fit in hold
    Cash = St#game.cash,
    Space = St#game.holdspace,
    M = St#game.localmarket,
    Price = index(I, M#market.prices),
    Q = index(I, M#market.quantities),
    Unit = (get_commodity(I))#comm.unit,
    T = if Cash =< 0 -> 0;
	   true ->
		T0 = min(Q, A),
		T1 = if Unit =:= ?tonnes -> min(Space, T0);
			true -> T0
		     end,
		min(T1, trunc(Cash / Price))
	end,
    M1 = M#market{quantities = update(I, M#market.quantities, Q - T)},
    St1 = St#game{localmarket = M1,
		  cash = Cash - T*Price,
		  holdspace = if Unit =:= ?tonnes -> Space - T;
				 true -> Space
			      end},
    {T, update_shipshold(I, T, St1)}.

%% As gamebuy but selling
gamesell(I, A, St) ->
    Cash = St#game.cash,
    Space = St#game.holdspace,
    Has = get_shipshold(I, St),
    T = min(Has, A),
    M = St#game.localmarket,
    Price = index(I, M#market.prices),
    Q = index(I, M#market.quantities) + T,
    M1 = M#market{quantities = update(I, M#market.quantities, Q)},
    Unit = (get_commodity(I))#comm.unit,
    St1 = St#game{localmarket = M1,
		  cash = Cash + T*Price,
		  holdspace = if Unit =:= ?tonnes -> Space + T;
				 true -> Space
			      end},
    {T, update_shipshold(I, -T, St1)}.

%%  Prices and availabilities are influenced by the planet's economy type
%%  (0-7) and a random "fluctuation" byte that was kept within the saved
%%  commander position to keep the market prices constant over gamesaves.
%%  Availabilities must be saved with the game since the player alters them
%%  by buying (and selling(?))
%%
%%  Almost all operations are one byte only and overflow "errors" are
%%  extremely frequent and exploited.
%%
%%  Trade Item prices are held internally in a single byte=true value/4.
%%  The decimal point in prices is introduced only when printing them.
%%  Internally, all prices are integers.
%%  The player's cash is held in four bytes. 

genmarket(Fluct, System) ->
    %%do_genmarket(Fluct, System, [], [], 0),
    {Qs, Ps} = lists:unzip([genmarket(Fluct, System, I)
			    || I <- lists:seq(0, ?lasttrade)]),
    #market{quantities = list_to_tuple(Qs), prices = list_to_tuple(Ps)}.

genmarket(Fluct, System, I) ->
    Cy = get_commodity(I),
    Product = (System#plansys.economy)*Cy#comm.gradient,
    Changing = Fluct band Cy#comm.maskbyte,
    Q0 = (Cy#comm.basequant + Changing - Product) band 16#FF,
    Q1 = if (Q0 band 16#80) -> 0;    %% Clip to positive 8-bit
	    true -> Q0
	 end,
    Q = (Q1 band 16#3F),  %% Mask to 6 bits
    P0 = (Cy#comm.baseprice + Changing + Product) band 16#FF,
    P = (P0*4) band 16#FFFF,
    if I =:= ?AlienItems ->
	    {0, P};  %% Override AlienItems to force nonavailability
       true ->
	    {Q, P}
    end.

%% Generate system info from seed

makesystem(Seed0) ->
    {W0, W1, W2} = Seed0,

    X = W1 bsr 8,
    Y = W0 bsr 8,

    Govtype = ((W1 bsr 3) band 7),  %% bits 3,4 &5 of W1
    Economy0 = ((W0 bsr 8) band 7),  %% bits 8,9 &A of W0
    Economy = if (Govtype =< 1) -> (Economy0 bor 2);
		 true -> Economy0
	      end,
    Techlev0 = ((W1 bsr 8) band 3) + (Economy bxor 7),
    Techlev1 = Techlev0 + (Govtype bsr 1),
    Techlev = if ((Govtype band 1) =:= 1) -> Techlev1 + 1;
		 true -> Techlev1
	      end,
    
    %% C simulation of 6502's LSR then ADC
 
    Population0 = 4*Techlev + Economy,
    Population = Population0 + (Govtype + 1),
    
    Productivity0 = ((Economy bxor 7) + 3) * (Govtype + 4),
    Productivity = Productivity0 * (Population * 8),

    Radius = 256*(((W2 bsr 8) band 15) + 11) + X,
    
    A = W1 band 16#FF,
    B = W1 bsr 8,
    C = W2 band 16#FF,
    D = W2 bsr 8,
    Goatsoupseed = {A, B, C, D},

    HumanColony = (C band 16#80) =:= 0,
    SpeciesAdj1 = (D bsr 2) band 3,
    SpeciesAdj2 = (D bsr 5) band 7,
    SpeciesAdj3 = (X bxor Y) band 7,
    SpeciesType = (SpeciesAdj3 + (D band 3)) band 7,

    Pair1 = 2*((W2 bsr 8) band 31),
    Seed1 = tweakseed(Seed0),
    {_, _, W22} = Seed1,
    Pair2 = 2*((W22 bsr 8) band 31),
    Seed2 = tweakseed(Seed1),
    {_, _, W23} = Seed2,
    Pair3 = 2*((W23 bsr 8) band 31),
    Seed3 = tweakseed(Seed2),
    {_, _, W24} = Seed3,
    Pair4 = 2*((W24 bsr 8) band 31),
    Seed4 = tweakseed(Seed3),
    %% Always four iterations of random number

    Pairs = pairs1(),
    Name0 = [get_pair(Pairs, Pair1),
	     get_pair(Pairs, Pair2),
	     get_pair(Pairs, Pair3)],

    %% bit 6 of ORIGINAL w0 flags a four-pair name
    Name1 = if (W0 band 64) =/= 0 ->
		    Name0 ++ get_pair(Pairs, Pair4);
	       true ->
		    Name0
	    end,
    Name = stripout(lists:flatten(Name1), $.),
    {#plansys{x = X, y = Y, economy = Economy, govtype = Govtype,
	      techlev = Techlev, population = Population,
	      productivity = Productivity, radius = Radius,
              human_colony = HumanColony, species_type = SpeciesType,
              species_adj1 = SpeciesAdj1, species_adj2 = SpeciesAdj2,
              species_adj3 = SpeciesAdj3,
	      goatsoupseed = Goatsoupseed, name = Name
	     },
     Seed4}.

%% Remove all C's from string
stripout([C | Cs], C) ->
    stripout(Cs, C);
stripout([C1 | Cs], C) ->
    [C1 | stripout(Cs, C)];
stripout([], _C) ->
    [].

%% seedtype C struct:
%%  uint16 w0   // six byte random number used as seed for planets
%%  uint16 w1
%%  uint16 w2

tweakseed({W0, W1, W2}) ->
    Temp = (W0 + W1 + W2) band 16#FFFF, %% 2 byte aritmetic
    {W1, W2, Temp}.

%% Generate galaxy

%% Functions for galactic hyperspace

%% rotate 8 bit number leftwards
rotatel(X) ->
    %% (tried to use chars but too much effort persuading this braindead
    %% language (C) to do bit operations on bytes!)
    (2*(X band 127)) + ((X band 128) bsr 7).

twist(X) ->
    (256*rotatel(X bsr 8)) + rotatel(X band 255).

%% Apply to base seed; once for galaxy 2
%% twice for galaxy 3, etc.
%% Eighth application gives galaxy 1 again
nextgalaxy({W0, W1, W2}) -> 
    {twist(W0), twist(W1), twist(W2)}.

%% Original game generated from scratch each time info needed
buildgalaxy(GalaxyNum) ->
    Seed1 = {?base0, ?base1, ?base2},  %% Initialise seed for galaxy 1
    SeedN = do_nextgalaxy(Seed1, GalaxyNum, 1),
    %% Put galaxy data into array of structures
    list_to_tuple(do_makesystems(SeedN, 0, [])).

do_nextgalaxy(Seed, N, I) when I < N ->
    do_nextgalaxy(nextgalaxy(Seed), N, I+1);
do_nextgalaxy(Seed, _, _) ->
    Seed.

do_makesystems(Seed, I, Ss) when I < ?galsize ->
    {S, Seed1} = makesystem(Seed),
    do_makesystems(Seed1, I+1, [S | Ss]);
do_makesystems(_Seed, _I, Ss) ->
    lists:reverse(Ss).

%% Functions for navigation

%% Move to system I
gamejump(I, St) ->
    St#game{currentplanet = I,
	    localmarket = genmarket(randbyte(), index(I, St#game.galaxy))}.

randbyte() ->
    myrand() band 16#FF.

distance(#plansys{x = Ax, y = Ay}, #plansys{x = Bx, y = By}) ->
    %% Seperation between two planets (4*sqrt(X*X+Y*Y/4))
    ftoi(4*math:sqrt((Ax-Bx)*(Ax-Bx)+(Ay-By)*(Ay-By)/4)).

%% Common part of pairs0() and pairs1()
%% Dots should be nullprint characters
-define(PAIRS,
	"..LEXEGEZACEBISO"
	"USESARMAINDIREA."
	"ERATENBERALAVETI"
	"EDORQUANTEISRION").

pairs0() -> <<"ABOUSEITILETSTONLONUTHNO" ?PAIRS>>.

pairs1() -> <<?PAIRS>>.

get_pair(Bin, N) ->
    binary_to_list(Bin, N+1, N+2).

desc_list() ->
    %% planetary descriptions
    %% (Erlang does not support \x-escapes, so I had to use octal!)
    %% \x80 = \200
    %% \x88 = \210
    %% \x8F = \217
    %% \x90 = \220
    %% \x98 = \230
    %% \x9F = \237
    %% \xA0 = \240
    %% \xB0 = \260
    {{<<"fabled">>, <<"notable">>, <<"well known">>, <<"famous">>, <<"noted">>}, %% 81 201
     {<<"very">>, <<"mildly">>, <<"most">>, <<"reasonably">>, <<"">>}, %% 82 202
     {<<"ancient">>, <<"\225">>, <<"great">>, <<"vast">>, <<"pink">>}, %% 83 203
     {<<"\236 \235 plantations">>, <<"mountains">>, <<"\234">>, <<"\224 forests">>, <<"oceans">>}, %% 84 204
     {<<"shyness">>, <<"silliness">>, <<"mating traditions">>, <<"loathing of \206">>, <<"love for \206">>}, %% 85 205
     {<<"food blenders">>, <<"tourists">>, <<"poetry">>, <<"discos">>, <<"\216">>}, %% 86 206
     {<<"talking tree">>, <<"crab">>, <<"bat">>, <<"lobster">>, <<"\262">>}, %% 87 207
     {<<"beset">>, <<"plagued">>, <<"ravaged">>, <<"cursed">>, <<"scourged">>}, %% 88 210
     {<<"\226 civil war">>, <<"\233 \230 \231s">>, <<"a \233 disease">>, <<"\226 earthquakes">>, <<"\226 solar activity">>}, %% 89 211
     {<<"its \203 \204">>, <<"the \261 \230 \231">>, <<"its inhabitants' \232 \205">>, <<"\241">>, <<"its \215 \216">>}, %% 8A 212
     {<<"juice">>, <<"brandy">>, <<"water">>, <<"brew">>, <<"gargle blasters">>}, %% 8B 213
     {<<"\262">>, <<"\261 \231">>, <<"\261 \262">>, <<"\261 \233">>, <<"\233 \262">>}, %% 8C 214
     {<<"fabulous">>, <<"exotic">>, <<"hoopy">>, <<"unusual">>, <<"exciting">>}, %% 8D 215
     {<<"cuisine">>, <<"night life">>, <<"casinos">>, <<"sit coms">>, <<" \241 <<">>}, %% 8E 216
     {<<"\260">>, <<"The planet \260">>, <<"The world \260">>, <<"This planet">>, <<"This world">>}, %% 8F 217
     {<<"n unremarkable">>, <<" boring">>, <<" dull">>, <<" tedious">>, <<" revolting">>}, %% 90 220
     {<<"planet">>, <<"world">>, <<"place">>, <<"little planet">>, <<"dump">>}, %% 91 221
     {<<"wasp">>, <<"moth">>, <<"grub">>, <<"ant">>, <<"\262">>}, %% 92 222
     {<<"poet">>, <<"arts graduate">>, <<"yak">>, <<"snail">>, <<"slug">>}, %% 93 223
     {<<"tropical">>, <<"dense">>, <<"rain">>, <<"impenetrable">>, <<"exuberant">>}, %% 94 224
     {<<"funny">>, <<"weird">>, <<"unusual">>, <<"strange">>, <<"peculiar">>}, %% 95 225
     {<<"frequent">>, <<"occasional">>, <<"unpredictable">>, <<"dreadful">>, <<"deadly">>}, %% 96 226
     {<<"\202 \201 for \212">>, <<"\202 \201 for \212 and \212">>, <<"\210 by \211">>, <<"\202 \201 for \212 but \210 by \211">>, <<"a\220 \221">>}, %% 97 227
     {<<"\233">>, <<"mountain">>, <<"edible">>, <<"tree">>, <<"spotted">>}, %% 98 230
     {<<"\237">>, <<"\240">>, <<"\207oid">>, <<"\223">>, <<"\222">>}, %% 99 231
     {<<"ancient">>, <<"exceptional">>, <<"eccentric">>, <<"ingrained">>, <<"\225">>}, %% 9A 232
     {<<"killer">>, <<"deadly">>, <<"evil">>, <<"lethal">>, <<"vicious">>}, %% 9B 233
     {<<"parking meters">>, <<"dust clouds">>, <<"ice bergs">>, <<"rock formations">>, <<"volcanoes">>}, %% 9C 234
     {<<"plant">>, <<"tulip">>, <<"banana">>, <<"corn">>, <<"\262weed">>}, %% 9D 235
     {<<"\262">>, <<"\261 \262">>, <<"\261 \233">>, <<"inhabitant">>, <<"\261 \262">>}, %% 9E 236
     {<<"shrew">>, <<"beast">>, <<"bison">>, <<"snake">>, <<"wolf">>}, %% 9F 237
     {<<"leopard">>, <<"cat">>, <<"monkey">>, <<"goat">>, <<"fish">>}, %% A0 240
     {<<"\214 \213">>, <<"\261 \237 \242">>, <<"its \215 \240 \242">>, <<"\243 \244">>, <<"\214 \213">>}, %% A1 241
     {<<"meat">>, <<"cutlet">>, <<"steak">>, <<"burgers">>, <<"soup">>}, %% A2 242
     {<<"ice">>, <<"mud">>, <<"Zero-G">>, <<"vacuum">>, <<"\261 ultra">>}, %% A3 243
     {<<"hockey">>, <<"cricket">>, <<"karate">>, <<"polo">>, <<"tennis">>} %% A4 244
    }.

%%  B0 260 = <planet name>
%%  B1 261 = <planet name>ian
%%  B2 262 = <random name>

%% fastseedtype C struct:
%%  uint8 a,b,c,d   // four byte random number used for planet description

gen_rnd_number({A0,B0,C0,D0}) ->
    C1 = (A0 * 2) band 16#FF,
    A = if (A0 > 127) -> C1 + C0 + 1;
	   true -> C1 + C0
	end,
    A1 = A band 16#FF,
    Carry = A div 256,
    B1 = (Carry + B0 + D0) band 16#FF,
    D1 = B0,
    {{A1,B1,C1,D1}, B1}.

option(N, T) when N >= 16#CC -> element(5, T);
option(N, T) when N >= 16#99 -> element(4, T);
option(N, T) when N >= 16#66 -> element(3, T);
option(N, T) when N >= 16#33 -> element(2, T);
option(_, T) -> element(1, T).

goat_soup(System) ->
    {_, As} = goat_soup(<<"\217 is \227.">>, System),
    lists:reverse(As).

goat_soup(Source, #plansys{}=System) ->
    Seed = System#plansys.goatsoupseed,
    goat_soup(Source, System, Seed, []).

goat_soup([C | Cs], System, Seed, As) ->
    case C of
	_ when C < 16#80 ->
	    goat_soup(Cs, System, Seed, putc(C, As));
	_ when C =< 16#A4 ->
	    {Seed1, N} = gen_rnd_number(Seed),
	    Source = option(N, index(C - 16#81, desc_list())),
	    {Seed2, As1} = goat_soup(Source, System, Seed1, As),
	    goat_soup(Cs, System, Seed2, As1);
	16#B0 ->
	    %% planet name
	    Ns = System#plansys.name,
	    goat_soup(Cs, System, Seed,
		      puts(stolower(tl(Ns)), putc(hd(Ns), As)));
	16#B1 ->
	    %% <planet name>ian
	    Ns = System#plansys.name,
	    goat_soup(Cs, System, Seed,
		      puts("ian", ian(tl(Ns), putc(hd(Ns), As))));
	16#B2 ->
	    %% random name
	    {Seed1, N} = gen_rnd_number(Seed),
	    {Seed2, As1} = rndname(N band 3, Seed1, As),
	    goat_soup(Cs, System, Seed2, As1);
	_ ->
	    throw({error, sprintf("<bad char in data [~.16B]>",[C])})
    end;
goat_soup(Bin, System, Seed, As) when is_binary(Bin) ->
    goat_soup(binary_to_list(Bin), System, Seed, As);
goat_soup(_Cs, _System, Seed, As) ->
    {Seed, As}.

ian([C | Cs], As) when Cs =/= [] ; ((C =/= $E) and (C =/= $I)) ->
    ian(Cs, putc(tolower(C), As));
ian([_ | Cs], As) -> ian(Cs, As);
ian([], As) -> As.

rndname(Len, Seed, As) ->
    rndname(Len, Seed, As, 0).

rndname(Len, Seed, As, I) when I =< Len ->
    {Seed1, N} = gen_rnd_number(Seed),
    [P1, P2] = get_pair(pairs0(), N band 16#3e),
    %% This part was corrected by me (Richard Carlsson). Ian Bell's
    %% txtelite.c 1.4 contained a spurious "i &&" condition on emitting
    %% the second character of a pair (e.g. causing it to generate "M
    %% corn" instead of "MA corn" for Diso). On the other hand, his code
    %% did not handle the translation to lowercase at all (possibly, the
    %% "i &&" was a remnant from some attempt at that). I have checked
    %% the code below against an emulated version and got the expected
    %% "Onbidi tulips" for Maregeis.
    As1 = if P1 =/= $. ->
                  if I =:= 0 -> putc(P1, As);
                     true -> putc(tolower(P1), As)
                  end;
	     true -> As
	  end,
    As2 = if P2 =/= $. ->
                  %% Leave as uppercase if we didn't print the first
                  %% character and this is the first pair
                  if P1 =:= $., I =:= 0 -> putc(P2, As1);
                     true -> putc(tolower(P2), As1)
                  end;
	     true -> As1
	  end,
    rndname(Len, Seed1, As2, I+1);
rndname(_Len, Seed, As, _I) ->
    {Seed, As}.

tolower(C) when C >= $A, C =< $Z -> C + ($a - $A);
tolower(C) -> C.

stolower(Cs) -> [tolower(C) || C <- Cs].

puts(S, Tail) ->
    lists:reverse(S, Tail).

putc(C, As) -> [C | As].

atoi(S) ->
    try list_to_integer(S)
    catch _:_ -> 0
    end.

atof(S) ->
    try list_to_integer(S)
    catch _:_ ->
	    try list_to_float(S)
	    catch _:_ -> 0
	    end
    end.

%%  ftoi
ftoi(Value) -> trunc(Value + 0.5).

rand() -> rand:uniform(32768) - 1.  %% emulated std C rand()

mysrand(Seed) ->
    %% Erlang stdlib seeding
    rand:seed(exsss, {Seed div 3, Seed div 5, Seed div 7}),
    put(lastrand, Seed - 1).

myrand() ->
    case get(nativerand) of
	true ->
	    rand();
	false ->
	    %% As supplied by D McDonnell from SAS Insititute C
	    Lastrand = get(lastrand),
	    R = (((((((((((Lastrand bsl 3) - Lastrand) bsl 3)
			+ Lastrand) bsl 1) + Lastrand) bsl 4)
		    - Lastrand) bsl 1) - Lastrand) + 16#e60)
		band 16#7fffffff,
	    put(lastrand, R - 1),
	    R
    end.

%% zero-based tuple indexing
index(I, T) -> element(I+1, T).
update(I, T, V) -> setelement(I+1, T, V).

sprintf(S, As) -> lists:flatten(io_lib:format(S, As)).

%% ========================================================================
%% Interface section

commands() ->
    {"buy",        "sell",     "fuel",     "jump",
     "cash",       "mkt",      "help",     "hold",
     "sneak",      "local",    "info",     "galhyp",
     "quit",       "rand"	
    }.

comfuncs(0, Ts, St) -> dobuy(Ts, St);
comfuncs(1, Ts, St) -> dosell(Ts, St);
comfuncs(2, Ts, St) -> dofuel(Ts, St);
comfuncs(3, Ts, St) -> dojump(Ts, St);
comfuncs(4, Ts, St) -> docash(Ts, St);
comfuncs(5, Ts, St) -> domkt(Ts, St);
comfuncs(6, Ts, St) -> dohelp(Ts, St);
comfuncs(7, Ts, St) -> dohold(Ts, St);
comfuncs(8, Ts, St) -> dosneak(Ts, St);
comfuncs(9, Ts, St) -> dolocal(Ts, St);
comfuncs(10, Ts, St) -> doinfo(Ts, St);
comfuncs(11, Ts, St) -> dogalhyp(Ts, St);
comfuncs(12, Ts, St) -> doquit(Ts, St);
comfuncs(13, Ts, St) -> dotweakrand(Ts, St).

doquit(_, _St) ->
    stop.

dohelp(_Ts, St) ->
    io:format("\nCommands are:"),
    io:format("\nBuy   tradegood amount"),
    io:format("\nSell  tradegood amount"),
    io:format("\nFuel  amount      (buy amount LY of fuel)"),
    io:format("\nJump  planetname  (limited by fuel)"),
    io:format("\nSneak planetname  (any distance - no fuel cost)"),
    io:format("\nGalhyp            (jumps to next galaxy)"),
    io:format("\nInfo  planetname  (prints info on system"),
    io:format("\nMkt               (shows market prices)"),
    io:format("\nLocal             (lists systems within 7 light years)"),
    io:format("\nCash number       (alters cash - cheating!)"),
    io:format("\nHold number       (change cargo bay)"),
    io:format("\nQuit or ^C        (exit)"),
    io:format("\nHelp              (display this text)"),
    io:format("\nRand              (toggle random number generator)"),
    io:format("\n\nAbbreviations allowed, e.g. b fo 5 = Buy Food 5, m = Mkt"),
    St.

%% Print data for given system
prisys_abbr(#plansys{}=Plsy) ->
    io:format("~10s", [Plsy#plansys.name]),
    io:format("  TL: ~2w  ", [Plsy#plansys.techlev + 1]),
    io:format("~20s", [get_econname(Plsy#plansys.economy)]),
    io:format(" ~16s", [get_govname(Plsy#plansys.govtype)]).

prisys_full(#plansys{}=Plsy) ->
    io:format("\n\nSystem:  "),
    io:format([hd(Plsy#plansys.name) | stolower(tl(Plsy#plansys.name))]),
    io:format("\nPosition (~w,", [Plsy#plansys.x]),
    io:format("~w)", [Plsy#plansys.y]),
    io:format("\nEconomy: (~w) ", [Plsy#plansys.economy]),
    io:format(get_econname(Plsy#plansys.economy)),
    io:format("\nGovernment: (~w) ", [Plsy#plansys.govtype]),
    io:format(get_govname(Plsy#plansys.govtype)),
    io:format("\nTech Level: ~2w", [Plsy#plansys.techlev + 1]),
    io:format("\nTurnover: ~w", [Plsy#plansys.productivity]),
    io:format("\nRadius: ~w", [Plsy#plansys.radius]),
    %% Correction from txtelite 1.4: divide population by 10, not by 8:
    io:format("\nPopulation: ~.1f Billion", [Plsy#plansys.population/10]),
    io:format("\nSpecies: "),
    case Plsy#plansys.human_colony of
        true ->
          io:format("Human Colonials\n");
        false ->
            if Plsy#plansys.species_adj1 < 3 ->
                    io:format("~s ", [get_species_stature(
                                        Plsy#plansys.species_adj1)]);
               true -> ok
            end,
            if Plsy#plansys.species_adj2 < 6 ->
                    io:format("~s ", [get_species_coloration(
                                        Plsy#plansys.species_adj2)]);
               true -> ok
            end,
            if Plsy#plansys.species_adj3 < 6 ->
                    io:format("~s ", [get_species_characteristic(
                                        Plsy#plansys.species_adj3)]);
               true -> ok
            end,
            io:format("~s\n", [get_species_base_type(
                                 Plsy#plansys.species_type)])
    end,
    io:format("\n"),
    io:put_chars(goat_soup(Plsy)).

%% Info on planet
doinfo([S | _], St) ->
    Dest = matchsys(S, St#game.currentplanet, St#game.galaxy),
    prisys_full(index(Dest, St#game.galaxy)),
    St;
doinfo(_, St) ->
    doinfo([""], St).

%% Return id of the planet whose name matches passed string
%% closest to currentplanet - if none return currentplanet
matchsys(S, Curr, Galaxy) ->
    matchsys(Galaxy, index(Curr, Galaxy), S, 0, Curr, 9999).

matchsys(Galaxy, Here, S, N, P, D) when N < ?galsize ->
    To = index(N, Galaxy),
    case stringbeg(S, To#plansys.name) andalso distance(To, Here) < D of
	true ->
	    matchsys(Galaxy, Here, S, N+1, N, distance(To, Here));
	false ->
	    matchsys(Galaxy, Here, S, N+1, P, D)
    end;
matchsys(_Galaxy, _Here, _S, _N, P, _D) ->
    P.

dolocal(_, St) ->
    io:format("Galaxy number ~w", [St#game.galaxynum]),
    Galaxy = St#game.galaxy,
    Here = index(St#game.currentplanet, Galaxy),
    dolocal(Galaxy, Here, St#game.fuel, 0),
    St.
    
dolocal(Galaxy, Here, Fuel, N) when N < ?galsize ->
    To = index(N, Galaxy),
    D = distance(To, Here),
    if D =< ?maxfuel ->
	    if D =:= 0 ->
		    io:format("\n + ");
	       D =< Fuel ->
		    io:format("\n * ");
	       true ->
		    io:format("\n - ")
	    end,
	    io:format("(~.1f LY) ", [D/10]),
	    prisys_abbr(To);
       true ->
	    ok
    end,
    dolocal(Galaxy, Here, Fuel, N+1);
dolocal(_, _, _, _) ->
    ok.

%% Jump to planet name S
dojump([S | _], St) ->
    Curr = St#game.currentplanet,
    Galaxy = St#game.galaxy,
    Dest = matchsys(S, Curr, Galaxy),
    if Dest =:= Curr ->
	    io:format("\nBad jump"),
	    St;
       true ->
	    D = distance(index(Dest, Galaxy), index(Curr, Galaxy)),
	    Fuel = St#game.fuel,
	    if D > Fuel ->
		    io:format("\nJump too far"),
		    St;
	       true ->
		    St1 = gamejump(Dest, St),
		    St2 = St1#game{fuel = Fuel - D},
		    prisys_full(index(St2#game.currentplanet, Galaxy)),
		    St2
	    end
    end;
dojump(_, St) ->
    io:format("\nWhere do you want to jump?"),
    St.

%% As dojump but no fuel cost
dosneak(Ts, St) ->
    St1 = dojump(Ts, St#game{fuel = 666}),
    St1#game{fuel=St#game.fuel}.

%% Jump to next galaxy
dogalhyp(_, St) ->
    %% Preserve planetnum (eg. if leave 7th planet, arrive at 7th planet)
    G0 = St#game.galaxynum,
    G = if G0 =:= 8 -> 1;
	   true -> G0 + 1
	end,
    St#game{galaxynum = G, galaxy = buildgalaxy(G)}.

dohold([S | _], St) ->
    A = atoi(S),
    T = lists:sum([get_shipshold(N, St)
		   || N <- lists:seq(0, ?lasttrade),
		      (get_commodity(N))#comm.unit =:= ?tonnes]),
    if (T > A) ->
	    io:format("\nHold too full"),
	    St;
       true ->
	    St#game{holdspace = A-T}
    end;
dohold(_, St) ->
    io:format("\nResetting hold to small cargo bay"),
    dohold(["20"], St).

dosell(Ts, St) ->
    dobuysell(Ts, St, "Sell", fun gamesell/3).

dobuy(Ts, St) ->
    dobuysell(Ts, St, "Buy", fun gamebuy/3).

%% Buy or sell amount S2 of good S1
dobuysell([S1, S2 | _], St, What, How) ->
    A0 = atoi(S2),
    A = if A0 =:= 0 -> 1;
	   true -> A0
	end,
    Names = tradnames(),
    N = stringmatch(S1, Names, ?lasttrade + 1),
    if N =:= 0 ->
	    io:format("\nUnknown trade good ~s", [S1]),
	    St;
       true ->
	    N1 = N - 1,
	    {T, St1} = How(N1, A, St),
	    if T =:= 0 ->
		    io:format("\nCannot ~s any ", [stolower(What)]);
	       true ->
		    io:format("\n~sing ~w", [What, T]),
		    io:format(get_unitname((get_commodity(N1))#comm.unit)),
		    io:format(" of ")
	    end,
	    io:format(element(N, Names)),
	    St1
    end;
dobuysell(_, St, What, _How) ->
    io:format("\nMust specify what to ~s and how much", [stolower(What)]),
    St.

%% Buy amount S of fuel
dofuel([S | _], St) ->
    {F, St1} = gamefuel(trunc(10*atof(S)), St),
    if F =:= 0 ->
	    io:format("\nCan't buy any fuel");
       true ->
	    io:format("\nBuying ~.1fLY fuel",[F/10])
    end,
    St1;
dofuel(_, St) ->
    dofuel(["99"], St).
    
%% Cheat alter cash by S
docash([S | _], St) ->
    A = trunc(10*atof(S)),
    if A =:= 0 ->
	    io:format("\nNumber not understood"),
	    St;
       true ->
	    St#game{cash = St#game.cash + A}
    end;
docash(_, St) ->
    docash(["+100"], St).

%% Show stock market
domkt(_, St) ->
    displaymarket(St#game.localmarket, St),
    io:format("\nHoldspace: ~wt",[St#game.holdspace]),
    St.

displaymarket(M, St) ->
    displaymarket(M, St, 0).

displaymarket(M, St, I) when I =< ?lasttrade ->
    Cy = get_commodity(I),
    io:format("\n~.12s", [Cy#comm.name]),
    io:format("   ~5.1fc", [(index(I, M#market.prices))/10]),
    io:format("   ~3w", [index(I, M#market.quantities)]),
    io:format("~.3s", [get_unitname(Cy#comm.unit)]),
    io:format("  ~w", [get_shipshold(I, St)]),
    displaymarket(M, St, I+1);
displaymarket(_M, _St, _I) ->
    ok.

dotweakrand(_, St) ->
    put(nativerand, get(nativerand) xor true),
    St.

%% *main*
main() ->
    %% 6502 Elite fires up at Lave with fluctuation=00
    %% and these prices tally with the NES ones.
    %% However, the availabilities reside in the saved game data.
    %% Availabilities are calculated (and fluctuation randomised)
    %% on hyperspacing
    %% I have checked with this code for Zaonce with fluctaution &AB 
    %% against the SuperVision 6502 code and both prices and availabilities
    %% tally.

    io:format("\nWelcome to Text Elite 1.5.\n"),

    put(nativerand, true),

    %% Ensure repeatability
    mysrand(12345),

    GalNum = 1,
    Galaxy = buildgalaxy(GalNum),
    Planet = ?numforLave,
    Market = genmarket(0, index(Planet, Galaxy)),  %% Since want seed=0

    St0 = #game{cash = 0,
		fuel = ?maxfuel,
		galaxynum = GalNum,
		currentplanet = Planet,    %% Don't use jump
		localmarket = Market,
		galaxy = Galaxy
	       },
    lists:foldl(
      fun (F, St) -> F(St) end,
      St0,
      [fun init_shipshold/1,
       fun (St) -> parser("hold 20", St) end,    %% Small cargo bay
       fun (St) -> parser("cash +100", St) end,  %% 100 CR
       fun (St) -> parser("help", St) end,
       fun main_loop/1]),
    ok.

main_loop(St) ->
    P = io_lib:format("\n\n(Cash: ~.1fc, Fuel: ~.1f LY)> ",
		      [St#game.cash / 10, St#game.fuel / 10]),
    case parser(io:get_line(P), St) of
	#game{}=St1 ->
	    main_loop(St1);
	stop ->
	    ok
    end.

%% Obey command S (returns false to quit, true to continue)
parser(S, St) ->
    Ts = string:tokens(S, "\s\t\n"),
    N = if Ts =/= [] ->
		stringmatch(hd(Ts), commands(), ?nocomms);
	   true ->
		0
	end,
    if N =/= 0 ->
	    comfuncs(N-1, tl(Ts), St);
       true ->
	    if Ts =/= [] -> io:format("\n Bad command (~s)",[hd(Ts)]);
	       true -> ok
	    end,
	    St
    end.

%% Check string s against n options in string array a
%% If matches ith element (starting at 1), return i else return 0
stringmatch(S, A, N) ->
    stringmatch(S, A, N, 1).

stringmatch(S, A, N, I) when I =< N ->
    case stringbeg(S, element(I, A)) of
	true -> I;
	false -> stringmatch(S, A, N, I+1)
    end;
stringmatch(_S, _A, _N, _I) ->
    0.

%% Return true iff string t begins with non-empty string s
stringbeg([], _T) -> false;
stringbeg(Ss, Ts) -> stringbeg1(Ss, Ts).

stringbeg1([S|Ss], [T|Ts]) ->
    case tolower(S) =:= tolower(T) of
	true -> stringbeg1(Ss, Ts);
	false -> false
    end;
stringbeg1([], _Ts) -> true;
stringbeg1(_Ss, _Ts) -> false.
