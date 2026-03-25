:: borrowed from ~migrev-dolseg's %hawk
::
|%
++  feather
  ;style
    ;                   /*  feather css  */
    ;                   /*  part 1: resets  */
    ; *,
    ; *::before,
    ; *::after {
    ;   box-sizing: border-box;
    ;   margin: 0;
    ; }
    ; html {
    ;   -moz-text-size-adjust: none;
    ;   -webkit-text-size-adjust: none;
    ;   text-size-adjust: none;
    ;   overflow: hidden;
    ; }
    ; body, h1, h2, h3, h4, p,
    ; figure, blockquote, dl, dd {
    ;   margin-block-end: 0;
    ; }
    ; ul[role='list'],
    ; ol[role='list'] {
    ;   list-style: none;
    ; }
    ; h1, h2, h3, h4,
    ; button, input, label {
    ;   line-height: 1.1;
    ; }
    ; button, summary {
    ;   cursor: pointer;
    ;   touch-action: manipulation;
    ; }
    ; details summary.no-arrow::-webkit-details-marker {
    ;   display: none;
    ; }
    ; h1, h2,
    ; h3, h4 {
    ;   text-wrap: balance;
    ; }
    ; a {
    ;   text-decoration: none;
    ;   color: currentColor;
    ; }
    ; img,
    ; picture {
    ;   max-width: 100%;
    ;   display: block;
    ; }
    ; input, button,
    ; textarea, select {
    ;   padding: 0;
    ;   font-family: inherit;
    ;   font-size: 1rem;
    ;   background: inherit;
    ;   border: unset;
    ;   border-radius: 0px;
    ;   color: inherit;
    ;   letter-spacing: inherit;
    ;   line-height: inherit;
    ;   outline: none; }
    ; :focus {
    ;   outline: none;
    ;   box-shadow: inset  1px  1px var(--focus-color),
    ;               inset -1px -1px var(--focus-color);
    ; }
    ; .no-outline:focus {
    ;   outline: none;
    ;   box-shadow: unset;
    ; }
    ; ::placeholder {
    ;   color: var(--f4);
    ;   opacity: 0.3;
    ;   }
    ; textarea { resize: none; }
    ; b { font-weight: bold; }
    ; i { font-style: italic; }
    ;                   /*  part 2: variables  */
    ; :root {
    ;   --font:         'Arial';
    ;   --font-mono:    monospace;
    ;   --mono-scale:   0.9;
    ;   --kerning:      0.024em;
    ;   --line-height:  1.4;
    ;   --0in:          calc(0 * var(--1in));
    ;   --1in:          4px;
    ;   --font-size:    calc(4 * var(--1in));
    ;   --2in:          calc(2 * var(--1in));
    ;   --3in:          calc(3 * var(--1in));
    ;   --4in:          calc(4 * var(--1in));
    ;   --5in:          calc(5 * var(--1in));
    ;   --6in:          calc(6 * var(--1in));
    ;   --7in:          calc(7 * var(--1in));
    ;   --8in:          calc(8 * var(--1in));
    ;   --9in:          calc(9 * var(--1in));
    ;   --10in:         calc(10 * var(--1in));
    ;   --11in:         calc(11 * var(--1in));
    ;   --12in:         calc(12 * var(--1in));
    ;   --13in:         calc(13 * var(--1in));
    ;   --14in:         calc(14 * var(--1in));
    ;   --15in:         calc(15 * var(--1in));
    ;   --16in:         calc(16 * var(--1in));
    ;   --17in:         calc(17 * var(--1in));
    ;   --18in:         calc(18 * var(--1in));
    ;   --19in:         calc(19 * var(--1in));
    ;   --20in:         calc(20 * var(--1in));
    ;   --21in:         calc(21 * var(--1in));
    ;   --22in:         calc(22 * var(--1in));
    ;   --23in:         calc(23 * var(--1in));
    ;   --24in:         calc(24 * var(--1in));
    ;   --25in:         calc(25 * var(--1in));
    ;   --26in:         calc(26 * var(--1in));
    ;   --27in:         calc(27 * var(--1in));
    ;   --28in:         calc(28 * var(--1in));
    ;   --29in:         calc(29 * var(--1in));
    ;   --30in:         calc(30 * var(--1in));
    ;   --31in:         calc(31 * var(--1in));
    ;   --32in:         calc(32 * var(--1in));
    ;   --33in:         calc(33 * var(--1in));
    ;   --34in:         calc(34 * var(--1in));
    ;   --35in:         calc(35 * var(--1in));
    ;   --36in:         calc(36 * var(--1in));
    ;   --37in:         calc(37 * var(--1in));
    ;   --38in:         calc(38 * var(--1in));
    ;   --39in:         calc(39 * var(--1in));
    ;   --40in:         calc(40 * var(--1in));
    ;   --p-page:       30px 15px 450px 15px;
    ;   --light-b-4:    #bbdefb;
    ;   --light-b-3:    #c8e6c9;
    ;   --light-b-2:    #fff9c4;
    ;   --light-b-1:    #ffccbc;
    ;   --light-b0:     #e8e8e8;
    ;   --light-b1:     #dfdfdf;
    ;   --light-b2:     #d3d3d3;
    ;   --light-b3:     #d0d0d0;
    ;   --light-b4:     #c8c8c8;
    ;   --light-f-4:    #334dde;
    ;   --light-f-3:    #286e2c;
    ;   --light-f-2:    #e87800;
    ;   --light-f-1:    #d32f2f;
    ;   --light-f0:     #111111;
    ;   --light-f1:     #333333;
    ;   --light-f2:     #444444;
    ;   --light-f3:     #555555;
    ;   --light-f4:     #777777;
    ;   --dark-b-4:     #1a237e;
    ;   --dark-b-3:     #1b5e20;
    ;   --dark-b-2:     #472e12;
    ;   --dark-b-1:     #560c0c;
    ;   --dark-b0:      #161616;
    ;   --dark-b1:      #222222;
    ;   --dark-b2:      #303030;
    ;   --dark-b3:      #383838;
    ;   --dark-b4:      #444444;
    ;   --dark-f-4:     #8899ff;
    ;   --dark-f-3:     #66f699;
    ;   --dark-f-2:     #ffeb6b;
    ;   --dark-f-1:     #ff6d40;
    ;   --dark-f0:      #eeeeee;
    ;   --dark-f1:      #cccccc;
    ;   --dark-f2:      #bbbbbb;
    ;   --dark-f3:      #aaaaaa;
    ;   --dark-f4:      #888888;
    ;   --winter-light-b1: #e2e7f0;
    ;   --winter-light-b2: #cdd4e1;
    ;   --winter-light-b3: #b8c1d3;
    ;   --winter-light-b4: #a3acc5;
    ;   --winter-light-f1: #3a3f52;
    ;   --winter-light-f2: #495067;
    ;   --winter-light-f3: #5c647b;
    ;   --winter-light-f4: #737b94;
    ;   --winter-dark-b1: #1c1f29;
    ;   --winter-dark-b2: #262a37;
    ;   --winter-dark-b3: #303644;
    ;   --winter-dark-b4: #3a4351;
    ;   --winter-dark-f1: #cfd9ea;
    ;   --winter-dark-f2: #b9c5df;
    ;   --winter-dark-f3: #a2b1d3;
    ;   --winter-dark-f4: #8b9dc7;
    ;   --spring-light-b1: #edf2e4;
    ;   --spring-light-b2: #d9e3d2;
    ;   --spring-light-b3: #c5d4c1;
    ;   --spring-light-b4: #b1c6b0;
    ;   --spring-light-f1: #394e3a;
    ;   --spring-light-f2: #4b6150;
    ;   --spring-light-f3: #5e7465;
    ;   --spring-light-f4: #71887b;
    ;   --spring-dark-b1: #1a211a;
    ;   --spring-dark-b2: #232b23;
    ;   --spring-dark-b3: #2d362d;
    ;   --spring-dark-b4: #374037;
    ;   --spring-dark-f1: #d0ead2;
    ;   --spring-dark-f2: #bde0c1;
    ;   --spring-dark-f3: #a9d6af;
    ;   --spring-dark-f4: #96cc9e;
    ;   --summer-light-b1: #fff0e1;
    ;   --summer-light-b2: #ffe0c2;
    ;   --summer-light-b3: #ffd1a3;
    ;   --summer-light-b4: #ffc184;
    ;   --summer-light-f1: #66422d;
    ;   --summer-light-f2: #805336;
    ;   --summer-light-f3: #99663f;
    ;   --summer-light-f4: #b27949;
    ;   --summer-dark-b1: #291c14;
    ;   --summer-dark-b2: #362419;
    ;   --summer-dark-b3: #442d1f;
    ;   --summer-dark-b4: #513625;
    ;   --summer-dark-f1: #ffd4ad;
    ;   --summer-dark-f2: #ffc292;
    ;   --summer-dark-f3: #ffb077;
    ;   --summer-dark-f4: #ff9e5d;
    ;   --autumn-light-b1: #f2ede3;
    ;   --autumn-light-b2: #e4dac7;
    ;   --autumn-light-b3: #d6c7ab;
    ;   --autumn-light-b4: #c9b58f;
    ;   --autumn-light-f1: #4a3a28;
    ;   --autumn-light-f2: #5d4a33;
    ;   --autumn-light-f3: #715b3f;
    ;   --autumn-light-f4: #856c4a;
    ;   --autumn-dark-b1: #1f1a14;
    ;   --autumn-dark-b2: #292218;
    ;   --autumn-dark-b3: #332b1d;
    ;   --autumn-dark-b4: #3d3421;
    ;   --autumn-dark-f1: #f0e0c7;
    ;   --autumn-dark-f2: #ddc7a8;
    ;   --autumn-dark-f3: #caae89;
    ;   --autumn-dark-f4: #b7956a;
    ; }
    ; :root {
    ;   --focus-color: #0000ff77;
    ;   --active-offset: 10%;
    ;   --percent-scale: 50%;
    ;   --b-4:          var(--light-b-4);
    ;   --b-3:          var(--light-b-3);
    ;   --b-2:          var(--light-b-2);
    ;   --b-1:          var(--light-b-1);
    ;   --b0:           var(--light-b0);
    ;   --b1:           var(--light-b1);
    ;   --b2:           var(--light-b2);
    ;   --b3:           var(--light-b3);
    ;   --b4:           var(--light-b4);
    ;   --f-4:          var(--light-f-4);
    ;   --f-3:          var(--light-f-3);
    ;   --f-2:          var(--light-f-2);
    ;   --f-1:          var(--light-f-1);
    ;   --f0:           var(--light-f0);
    ;   --f1:           var(--light-f1);
    ;   --f2:           var(--light-f2);
    ;   --f3:           var(--light-f3);
    ;   --f4:           var(--light-f4);
    ;   .winter {
    ;     --b1: var(--winter-light-b1);
    ;     --b2: var(--winter-light-b2);
    ;     --b3: var(--winter-light-b3);
    ;     --b4: var(--winter-light-b4);
    ;     --f1: var(--winter-light-f1);
    ;     --f2: var(--winter-light-f2);
    ;     --f3: var(--winter-light-f3);
    ;     --f4: var(--winter-light-f4);
    ;   }
    ;   .spring {
    ;     --b1: var(--spring-light-b1);
    ;     --b2: var(--spring-light-b2);
    ;     --b3: var(--spring-light-b3);
    ;     --b4: var(--spring-light-b4);
    ;     --f1: var(--spring-light-f1);
    ;     --f2: var(--spring-light-f2);
    ;     --f3: var(--spring-light-f3);
    ;     --f4: var(--spring-light-f4);
    ;   }
    ;   .summer {
    ;     --b1: var(--summer-light-b1);
    ;     --b2: var(--summer-light-b2);
    ;     --b3: var(--summer-light-b3);
    ;     --b4: var(--summer-light-b4);
    ;     --f1: var(--summer-light-f1);
    ;     --f2: var(--summer-light-f2);
    ;     --f3: var(--summer-light-f3);
    ;     --f4: var(--summer-light-f4);
    ;   }
    ;   .autumn {
    ;     --b1: var(--autumn-light-b1);
    ;     --b2: var(--autumn-light-b2);
    ;     --b3: var(--autumn-light-b3);
    ;     --b4: var(--autumn-light-b4);
    ;     --f1: var(--autumn-light-f1);
    ;     --f2: var(--autumn-light-f2);
    ;     --f3: var(--autumn-light-f3);
    ;     --f4: var(--autumn-light-f4);
    ;   }
    ; }
    ; @media (prefers-color-scheme: dark) {
    ;   :root {
    ;     --focus-color: #6666ff77;
    ;     --active-offset: 10%;
    ;     --percent-scale: 150%;
    ;     --b-4:        var(--dark-b-4);
    ;     --b-3:        var(--dark-b-3);
    ;     --b-2:        var(--dark-b-2);
    ;     --b-1:        var(--dark-b-1);
    ;     --b0:         var(--dark-b0);
    ;     --b1:         var(--dark-b1);
    ;     --b2:         var(--dark-b2);
    ;     --b3:         var(--dark-b3);
    ;     --b4:         var(--dark-b4);
    ;     --f-4:        var(--dark-f-4);
    ;     --f-3:        var(--dark-f-3);
    ;     --f-2:        var(--dark-f-2);
    ;     --f-1:        var(--dark-f-1);
    ;     --f0:         var(--dark-f0);
    ;     --f1:         var(--dark-f1);
    ;     --f2:         var(--dark-f2);
    ;     --f3:         var(--dark-f3);
    ;     --f4:         var(--dark-f4);
    ;     .winter {
    ;       --b1: var(--winter-dark-b1);
    ;       --b2: var(--winter-dark-b2);
    ;       --b3: var(--winter-dark-b3);
    ;       --b4: var(--winter-dark-b4);
    ;       --f1: var(--winter-dark-f1);
    ;       --f2: var(--winter-dark-f2);
    ;       --f3: var(--winter-dark-f3);
    ;       --f4: var(--winter-dark-f4);
    ;     }
    ;     .spring {
    ;       --b1: var(--spring-dark-b1);
    ;       --b2: var(--spring-dark-b2);
    ;       --b3: var(--spring-dark-b3);
    ;       --b4: var(--spring-dark-b4);
    ;       --f1: var(--spring-dark-f1);
    ;       --f2: var(--spring-dark-f2);
    ;       --f3: var(--spring-dark-f3);
    ;       --f4: var(--spring-dark-f4);
    ;     }
    ;     .summer {
    ;       --b1: var(--summer-dark-b1);
    ;       --b2: var(--summer-dark-b2);
    ;       --b3: var(--summer-dark-b3);
    ;       --b4: var(--summer-dark-b4);
    ;       --f1: var(--summer-dark-f1);
    ;       --f2: var(--summer-dark-f2);
    ;       --f3: var(--summer-dark-f3);
    ;       --f4: var(--summer-dark-f4);
    ;     }
    ;     .autumn {
    ;       --b1: var(--autumn-dark-b1);
    ;       --b2: var(--autumn-dark-b2);
    ;       --b3: var(--autumn-dark-b3);
    ;       --b4: var(--autumn-dark-b4);
    ;       --f1: var(--autumn-dark-f1);
    ;       --f2: var(--autumn-dark-f2);
    ;       --f3: var(--autumn-dark-f3);
    ;       --f4: var(--autumn-dark-f4);
    ;     }
    ;   }
    ; }
    ; @media (max-width: 700px) {
    ;   /* hover makes no sense on mobile */
    ;   .hover:hover {
    ;     filter: unset !important;
    ;   }
    ;   .hover.active {
    ;     filter: invert(var(--active-offset)) !important;
    ;   }
    ;   .hover.active:hover {
    ;     filter: invert(var(--active-offset)) !important;
    ;   }
    ; }
    ;                   /*  part 3: page styling  */
    ; html            { height: 100%;
    ;                   font-family: sans-serif; }
    ; body            { margin: 0;
    ;                   height: 100%;
    ;                   color: var(--f0);
    ;                   overflow: hidden;
    ;                   background: var(--b0);
    ;                   font-feature-settings: normal;
    ;                   font-variation-settings: normal;
    ;                   min-height: 100%;
    ;                   font-family: var(--font, sans-serif);
    ;                   letter-spacing: var(--letter-spacing);
    ;                   line-height: var(--line-height); }
    ;                   /*  part 4: utility classes  */
    ; .break          { word-break: break-word; }
    ; .break-word     { word-break: break-word; }
    ; .break-all      { word-break: break-all; }
    ; .break-none     { word-break: keep-all; }
    ; .nowrap         { white-space: nowrap; }
    ; .action         { touch-action: manipulation; }
    ; .hidden         { display: none !important; }
    ; *[hidden]       { display: none !important; }
    ; .jc             { justify-content: center; }
    ; .jb             { justify-content: space-between; }
    ; .ja             { justify-content: space-around; }
    ; .js             { justify-content: start; }
    ; .je             { justify-content: end; }
    ; .js             { justify-content: start; }
    ; .as             { align-items: start; }
    ; .af             { align-items: stretch; }
    ; .ae             { align-items: end; }
    ; .ac             { align-items: center; }
    ; .wfc            { width: fit-content; }
    ; .wf             { width: 100%; }
    ; .wn             { width: 0; }
    ; .mwf            { max-width: 100%; }
    ; .mw-page        { max-width: 650px; width: 100%; }
    ; .hf             { height: 100%; }
    ; .mhf            { max-height: 100%; }
    ; .hfc            { height: fit-content; }
    ; .h0             { height: 0px; }
    ; .h1             { height: 8px; }
    ; .h2             { height: 18px; }
    ; .h3             { height: 36px; }
    ; .h4             { height: 76px; }
    ; .h5             { height: 136px; }
    ; .h6             { height: 216px; }
    ; .h7             { height: 336px; }
    ; .h8             { height: 456px; }
    ; .h9             { height: 586px; }
    ; .h10            { height: 777px; }
    ; .contain        { object-fit: contain; }
    ; .mono           { font-family: var(--font-mono, monospace);
    ;                   font-size: calc(1em * var(--mono-scale)); }
    ; .italic         { font-style: italic; }
    ; .underline      { text-decoration: underline; }
    ; .nounderline      { text-decoration: none !important; }
    ; .bold           { font-weight: bold; }
    ; .strike         { text-decoration: line-through; }
    ; .pre            { white-space: pre; }
    ; .pre-line       { white-space: pre-line; }
    ; .tl             { text-align: left; }
    ; .tc             { text-align: center; }
    ; .tr             { text-align: right; }
    ; .block          { display: block; }
    ; .inline         { display: inline-block }
    ; .fc             { display: flex;
    ;                   flex-direction: column; }
    ; .fcr            { display: flex;
    ;                   flex-direction: column-reverse; }
    ; .fr             { display: flex;
    ;                   flex-direction: row; }
    ; .frw            { display: flex;
    ;                   flex-direction: row;
    ;                   flex-wrap: wrap; }
    ; .frrw           { display: flex;
    ;                   flex-direction: row-reverse;
    ;                   flex-wrap: wrap; }
    ; .frr            { display: flex;
    ;                   flex-direction: row-reverse; }
    ; .basis-full     { flex-basis: 100%; }
    ; .basis-half     { flex-basis: 50%; flex-shrink: 0; }
    ; .basis-none     { flex-basis: 0%; flex-shrink: 1; }
    ; .shrink-0       { flex-shrink: 0; }
    ; .relative       { position: relative; }
    ; .absolute       { position: absolute; }
    ; .fixed          { position: fixed; }
    ; .sticky         { position: sticky; }
    ; .z-2            { z-index: -20; }
    ; .z-1            { z-index: -10; }
    ; .z0             { z-index: 0; }
    ; .z1             { z-index: 10; }
    ; .z2             { z-index: 20; }
    ; .grow           { flex-grow: 1; }
    ; .g0             { gap: 0; }
    ; .g1             { gap: 4px; }
    ; .g2             { gap: 8px; }
    ; .g3             { gap: 12px; }
    ; .g4             { gap: 16px; }
    ; .g5             { gap: 20px; }
    ; .g6             { gap: 24px; }
    ; .g7             { gap: 32px; }
    ; .g8             { gap: 40px; }
    ; .p-8            { padding: 32px 64px; }
    ; .p-7            { padding: 28px 56px; }
    ; .p-6            { padding: 24px 48px; }
    ; .p-5            { padding: 20px 40px; }
    ; .p-4            { padding: 16px 32px; }
    ; .p-3            { padding: 12px 24px; }
    ; .p-2            { padding: 8px 16px; }
    ; .p-1            { padding: 4px 8px; }
    ; .p0             { padding: 0; }
    ; .p1             { padding: 4px; }
    ; .p2             { padding: 8px; }
    ; .p3             { padding: 12px; }
    ; .p4             { padding: 16px; }
    ; .p5             { padding: 24px; }
    ; .p6             { padding: 30px; }
    ; .p7             { padding: 34px; }
    ; .p8             { padding: 38px; }
    ; .p-page         { padding: var(--p-page); }
    ; .ma             { margin: auto; }
    ; .mt-1           { margin-top: 0.5rem; }
    ; .mt1            { margin-top: 1rem; }
    ; .mt2            { margin-top: 2rem; }
    ; .mt3            { margin-top: 3rem; }
    ; .mb-1           { margin-bottom: 0.5rem; }
    ; .mb1            { margin-bottom: 1rem; }
    ; .mb2            { margin-bottom: 2rem; }
    ; .mb3            { margin-bottom: 3rem; }
    ; .ml-1           { margin-left: 0.5rem; }
    ; .ml1            { margin-left: 1rem; }
    ; .ml2            { margin-left: 2rem; }
    ; .ml3            { margin-left: 3rem; }
    ; .mr-1           { margin-right: 0.5rem; }
    ; .mr1            { margin-right: 1rem; }
    ; .mr2            { margin-right: 2rem; }
    ; .mr3            { margin-right: 3rem; }
    ; .m0             { margin: 0; }
    ; .o0             { opacity: 0%; }
    ; .o1             { opacity: 10%; }
    ; .o2             { opacity: 20%; }
    ; .o3             { opacity: 30%; }
    ; .o4             { opacity: 40%; }
    ; .o5             { opacity: 50%; }
    ; .o6             { opacity: 60%; }
    ; .o7             { opacity: 70%; }
    ; .o8             { opacity: 80%; }
    ; .o9             { opacity: 90%; }
    ; .o10            { opacity: 100%; }
    ; .scroll-y       { overflow-y: auto; }
    ; .scroll-x       { overflow-x: auto; }
    ; .scroll-none    { overflow: hidden; }
    ; ::-webkit-scrollbar {
    ;     width: 12px;
    ; }
    ; ::-webkit-scrollbar-thumb {
    ;   background-color: var(--b4);
    ;   filter: brightness(var(--percent-scale));
    ; }
    ; ::-webkit-scrollbar-track {
    ;   background: var(--b1);
    ;   filter: brightness(var(--percent-scale));
    ;   border: 0.5px solid var(--b3);
    ; }
    ; ::-webkit-scrollbar-corner {
    ;   background: var(--b1);
    ;   filter: brightness(var(--percent-scale));
    ; }
    ; .f-4            { color: var(--f-4); }
    ; .f-3            { color: var(--f-3); }
    ; .f-2            { color: var(--f-2); }
    ; .f-1            { color: var(--f-1); }
    ; .f0             { color: var(--f0); }
    ; .f1             { color: var(--f1); }
    ; .f2             { color: var(--f2); }
    ; .f3             { color: var(--f3); }
    ; .f4             { color: var(--f4); }
    ; .b-none         { background-color: none; }
    ; .b-4            { background-color: var(--b-4); }
    ; .b-3            { background-color: var(--b-3); }
    ; .b-2            { background-color: var(--b-2); }
    ; .b-1            { background-color: var(--b-1); }
    ; .b0             { background-color: var(--b0); }
    ; .b1             { background-color: var(--b1); }
    ; .b2             { background-color: var(--b2); }
    ; .b3             { background-color: var(--b3); }
    ; .b4             { background-color: var(--b4); }
    ; .s-2            { font-size: 0.7rem; }
    ; .s-1            { font-size: 0.85rem; }
    ; .s0             { font-size: 1rem; }
    ; .s1             { font-size: 1.15rem; }
    ; .s2             { font-size: 1.3rem; }
    ; .s3             { font-size: 1.45rem; }
    ; .s4             { font-size: 1.6rem; }
    ; .s5             { font-size: 2rem; }
    ; .s6             { font-size: 2.4rem; }
    ; .bd0            { border: none; }
    ; .bd1            { border: 0.9px solid var(--b4); }
    ; .bd2            { border: 1.5px solid var(--f3); }
    ; .bd3            { border: 2.1px solid var(--f1); }
    ; .br0            { border-radius: 0px; }
    ; .br1            { border-radius: 3px; }
    ; .br2            { border-radius: 6px; }
    ; .br3            { border-radius: 12px; }
    ; .bc-4           { border-color: var(--f-4); }
    ; .bc-3           { border-color: var(--f-3); }
    ; .bc-2           { border-color: var(--f-2); }
    ; .bc-1           { border-color: var(--f-1); }
    ; .bc0            { border-color: var(--f0); }
    ; .bc1            { border-color: var(--f1); }
    ; .bc2            { border-color: var(--f2); }
    ; .bc3            { border-color: var(--f3); }
    ; .bc4            { border-color: var(--f4); }
    ; .toggled        { background: var(--f0);
    ;                   color: var(--b0); }
    ; .toggled.b0     { background-color: var(--f0); }
    ; .toggled.b1     { background-color: var(--f1); }
    ; .toggled.b2     { background-color: var(--f2); }
    ; .toggled.b3     { background-color: var(--f3); }
    ; .toggled.b4     { background-color: var(--f4); }
    ; .toggled.b-1    { background-color: var(--f-1); }
    ; .toggled.b-2    { background-color: var(--f-2); }
    ; .toggled.b-3    { background-color: var(--f-3); }
    ; .toggled.b-4    { background-color: var(--f-4); }
    ; .toggled.f0     { color: var(--b0); }
    ; .toggled.f1     { color: var(--b1); }
    ; .toggled.f2     { color: var(--b2); }
    ; .toggled.f3     { color: var(--b3); }
    ; .toggled.f4     { color: var(--b4); }
    ; .toggled.f-1    { color: var(--b-1); }
    ; .toggled.f-2    { color: var(--b-2); }
    ; .toggled.f-3    { color: var(--b-3); }
    ; .toggled.f-4    { color: var(--b-4); }
    ; .active         { filter: invert(var(--active-offset)); }
    ; *:disabled      { opacity: 0.4; cursor: default; }
    ; .hover:hover    { filter: invert(17%); }
    ; .hover.active:hover { filter: invert(25%); }
    ; .hover:disabled { filter: none; }
    ; .pointer        { cursor: pointer; }
    ; .grabber        { cursor: grab; }
    ; .no-select      { user-select: none;
    ;                   -webkit-user-select: none; }
    ; .animate-spin {
    ;   animation: spin 1s linear infinite;
    ; }
    ; @keyframes spin {
    ;   from { transform: rotate(0deg); }
    ;   to { transform: rotate(360deg); }
    ; }
    ; .page {
    ;   padding: var(--p-page);
    ;   margin: auto;
    ;   max-width: 650px;
    ; }
    ; .prose h1 {
    ;   font-size: 1.45rem;
    ;   margin: 1rem 0;
    ; }
    ; .prose h2 {
    ;   font-size: 1.3rem;
    ;   margin: 1rem 0;
    ; }
    ; .prose h3 {
    ;   font-size: 1.15rem;
    ;   margin: 1rem 0;
    ; }
    ; .prose h1, .prose h2, .prose h3 {
    ;   font-weight: bold;
    ; }
    ; .prose p {
    ;   margin: 1rem 0;
    ;   line-height: 1.5;
    ;   overflow-wrap: break-word;
    ; }
    ; .prose img {
    ;   margin: 1rem 0;
    ;   max-width: 100%;
    ;   display: block;
    ;   max-height: 350px;
    ; }
    ; .prose details {
    ;   margin: 1rem 0;
    ; }
    ; .prose a {
    ;   text-decoration: underline;
    ; }
    ; .prose blockquote {
    ;   margin: 1rem 0;
    ;   margin-left: 10px;
    ;   border-left: 2px solid var(--b3);
    ;   padding: 4px;
    ;   padding-left: 12px;
    ;   color: var(--f2);
    ; }
    ; .prose pre {
    ;   font-family: var(--font-mono, monospace);
    ;   font-size: calc(1em * var(--mono-scale));
    ;   overflow-x: auto;
    ;   width: 100%;
    ;   display: block;
    ;   padding: 8px;
    ;   margin: 1rem 0;
    ;   background-color: var(--b1);
    ; }
    ; .prose code {
    ;   font-family: var(--font-mono, monospace);
    ;   font-size: calc(1em * var(--mono-scale));
    ;   color: var(--f2);
    ; }
    ; .prose hr {
    ;   margin: 2rem 0;
    ; }
    ; .prose > ul,
    ; .prose > ol {
    ;   margin: 1rem 0;
    ; }
    ; .prose ul,
    ; .prose ol {
    ;   padding-left: 29px;
    ;   margin: 0;
    ; }
    ; .prose ul p,
    ; .prose ol p {
    ;   margin: 0.5rem 0;
    ;   line-height: calc(calc(1 + var(--line-height)) / 2);
    ; }
    ; .prose ul p:first-child,
    ; .prose ol p:first-child {
    ;   margin: 0;
    ;   margin-bottom: 0.5rem;
    ; }
    ; .prose ul p:last-child,
    ; .prose ol p:last-child {
    ;   margin: 0;
    ;   margin-top: 0.5rem;
    ; }
    ; .prose li {
    ;   margin: 0.5rem 0;
    ; }
    ; .prose ul ul,
    ; .prose ol ul,
    ; .prose ul ol,
    ; .prose ol ol {
    ;   margin-bottom: 0;
    ; }
    ; .prose summary {
    ;   user-select: none;
    ;   -webkit-user-select: none;
    ; }
    ; .prose table {
    ;   border-collapse: collapse;
    ;   border-radius: 0.3em;
    ;   th, td {
    ;     border: 1px solid var(--f3);
    ;     padding: 0.5em 1em;
    ;   }
    ; }
    ; .loader { position: relative; }
    ; .loading {
    ;   position: absolute;
    ;   top: 50%;
    ;   left: 50%;
    ;   transform: translate(-50%, -50%);
    ;   pointer-events: none;
    ; }
    ; .loader .loading {
    ;   opacity: 0;
    ;   transition: opacity 300ms;
    ; }
    ; .htmx-request.loader {
    ;   pointer-events: none;
    ; }
    ; .htmx-request .loader .loading,
    ; .loader.htmx-request .loading {
    ;   opacity: 1;
    ; }
    ; .loader .loaded {
    ;   opacity: 1;
    ;   transition: opacity 300ms;
    ; }
    ; .htmx-request .loader .loaded,
    ; .loader.htmx-request .loaded {
    ;   opacity: 0;
    ; }
    ;                   /* part 5: input styling */
    ; input[type=range] {
    ;   -webkit-appearance: none;
    ;   width: 100%;
    ;   height: 5px;
    ;   background: var(--f4);
    ;   outline: none;
    ;   border-radius: 5px;
    ; }
    ; /* Style the slider thumb */
    ; input[type=range]::-webkit-slider-thumb {
    ;   -webkit-appearance: none;
    ;   appearance: none;
    ;   width: 0.8em;
    ;   height: 0.8em;
    ;   background: var(--f0);
    ;   cursor: pointer;
    ;   border-radius: 50%;
    ;   border: none;
    ; }
    ; input[type=range]::-moz-range-thumb {
    ;   width: 0.8em;
    ;   height: 0.8em;
    ;   background: var(--f0);
    ;   cursor: pointer;
    ;   border-radius: 50%;
    ;   border: none;
    ; }
  ==
--
