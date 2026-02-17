#lang racket/base
(require "../2htdp/image.rkt")

(provide X11-AS-RACKET-HASH)


(define color-hex-list '(aliceblue 	f0f8ff
                                    antiquewhite 	faebd7
                                    antiquewhite1 	ffefdb
                                    antiquewhite2 	eedfcc
                                    antiquewhite3 	cdc0b0
                                    antiquewhite4 	8b8378
                                    aquamarine1 	7fffd4
                                    aquamarine2 	76eec6
                                    aquamarine4 	458b74
                                    azure1 	f0ffff
                                    azure2 	e0eeee
                                    azure3 	c1cdcd
                                    azure4 	838b8b
                                    beige 	f5f5dc
                                    bisque1 	ffe4c4
                                    bisque2 	eed5b7
                                    bisque3 	cdb79e
                                    bisque4 	8b7d6b
                                    black 	"000000"
                                    blanchedalmond 	ffebcd
                                    blue1 	0000ff
                                    blue2 	0000ee
                                    blue4 	00008b
                                    blueviolet 	8a2be2
                                    brown 	a52a2a
                                    brown1 	ff4040
                                    brown2 	ee3b3b
                                    brown3 	cd3333
                                    brown4 	8b2323
                                    burlywood 	deb887
                                    burlywood1 	ffd39b
                                    burlywood2 	eec591
                                    burlywood3 	cdaa7d
                                    burlywood4 	8b7355
                                    cadetblue 	5f9ea0
                                    cadetblue1 	98f5ff
                                    cadetblue2 	8ee5ee
                                    cadetblue3 	7ac5cd
                                    cadetblue4 	53868b
                                    chartreuse1 	7fff00
                                    chartreuse2 	76ee00
                                    chartreuse3 	66cd00
                                    chartreuse4 	458b00
                                    chocolate 	d2691e
                                    chocolate1 	ff7f24
                                    chocolate2 	ee7621
                                    chocolate3 	cd661d
                                    coral 	ff7f50
                                    coral1 	ff7256
                                    coral2 	ee6a50
                                    coral3 	cd5b45
                                    coral4 	8b3e2f
                                    cornflowerblue 	6495ed
                                    cornsilk1 	fff8dc
                                    cornsilk2 	eee8cd
                                    cornsilk3 	cdc8b1
                                    cornsilk4 	8b8878
                                    cyan1 	00ffff
                                    cyan2 	00eeee
                                    cyan3 	00cdcd
                                    cyan4 	008b8b
                                    darkgoldenrod 	b8860b
                                    darkgoldenrod1 	ffb90f
                                    darkgoldenrod2 	eead0e
                                    darkgoldenrod3 	cd950c
                                    darkgoldenrod4 	8b6508
                                    darkgreen 	"006400"
                                    darkkhaki 	bdb76b
                                    darkolivegreen 	556b2f
                                    darkolivegreen1 	caff70
                                    darkolivegreen2 	bcee68
                                    darkolivegreen3 	a2cd5a
                                    darkolivegreen4 	6e8b3d
                                    darkorange 	ff8c00
                                    darkorange1 	ff7f00
                                    darkorange2 	ee7600
                                    darkorange3 	cd6600
                                    darkorange4 	8b4500
                                    darkorchid 	9932cc
                                    darkorchid1 	bf3eff
                                    darkorchid2 	b23aee
                                    darkorchid3 	9a32cd
                                    darkorchid4 	68228b
                                    darksalmon 	e9967a
                                    darkseagreen 	8fbc8f
                                    darkseagreen1 	c1ffc1
                                    darkseagreen2 	b4eeb4
                                    darkseagreen3 	9bcd9b
                                    darkseagreen4 	698b69
                                    darkslateblue 	483d8b
                                    darkslategray 	2f4f4f
                                    darkslategray1 	97ffff
                                    darkslategray2 	8deeee
                                    darkslategray3 	79cdcd
                                    darkslategray4 	528b8b
                                    darkturquoise 	00ced1
                                    darkviolet 	"9400d3"
                                    deeppink1 	ff1493
                                    deeppink2 	ee1289
                                    deeppink3 	cd1076
                                    deeppink4 	8b0a50
                                    deepskyblue1 	00bfff
                                    deepskyblue2 	00b2ee
                                    deepskyblue3 	009acd
                                    deepskyblue4 	00688b
                                    dimgray 	"696969"
                                    dodgerblue1 	1e90ff
                                    dodgerblue2 	1c86ee
                                    dodgerblue3 	1874cd
                                    dodgerblue4 	104e8b
                                    firebrick 	b22222
                                    firebrick1 	ff3030
                                    firebrick2 	ee2c2c
                                    firebrick3 	cd2626
                                    firebrick4 	8b1a1a
                                    floralwhite 	fffaf0
                                    forestgreen 	228b22
                                    gainsboro 	dcdcdc
                                    ghostwhite 	f8f8ff
                                    gold1 	ffd700
                                    gold2 	eec900
                                    gold3 	cdad00
                                    gold4 	8b7500
                                    goldenrod 	daa520
                                    goldenrod1 	ffc125
                                    goldenrod2 	eeb422
                                    goldenrod3 	cd9b1d
                                    goldenrod4 	8b6914
                                    gray 	bebebe
                                    gray1 	"030303"
                                    gray2 	"050505"
                                    gray3 	"080808"
                                    gray4 	0a0a0a
                                    gray5 	0d0d0d
                                    gray6 	0f0f0f
                                    gray7 	"121212"
                                    gray8   "141414"
                                    gray9 	"171717"
                                    gray10 	1a1a1a
                                    gray11 	1c1c1c
                                    gray12 	1f1f1f
                                    gray13 	"212121"
                                    gray14 	"242424"
                                    gray15 	"262626"
                                    gray16 	"292929"
                                    gray17 	2b2b2b
                                    gray18 	2e2e2e
                                    gray19 	"303030"
                                    gray20 	"333333"
                                    gray21 	"363636"
                                    gray22 	"383838"
                                    gray23 	3b3b3b
                                    gray24 	3d3d3d
                                    gray25 	"404040"
                                    gray26 	"424242"
                                    gray27 	"454545"
                                    gray28 	"474747"
                                    gray29 	4a4a4a
                                    gray30 	4d4d4d
                                    gray31 	4f4f4f
                                    gray32 	"525252"
                                    gray33  "545454"
                                    gray34 	"575757"
                                    gray35 	"595959"
                                    gray36 	5c5c5c
                                    gray37 	5e5e5e
                                    gray38 	"616161"
                                    gray39 "636363"
                                    gray40 	"666666"
                                    gray41 	"696969"
                                    gray42 	6b6b6b
                                    gray43 	6e6e6e
                                    gray44 	"707070"
                                    gray45 	"737373"
                                    gray46 	"757575"
                                    gray47 "787878"
                                    gray48 	7a7a7a
                                    gray49 	7d7d7d
                                    gray50 	7f7f7f
                                    gray51 	"828282"
                                    gray52 	"858585"
                                    gray53 	"878787"
                                    gray54 	8a8a8a
                                    gray55 	8c8c8c
                                    gray56 	8f8f8f
                                    gray57 	"919191"
                                    gray58 	"949494"
                                    gray59 	"969696"
                                    gray60 	"999999"
                                    gray61 	9c9c9c
                                    gray62 	9e9e9e
                                    gray63 	a1a1a1
                                    gray64 	a3a3a3
                                    gray65 	a6a6a6
                                    gray66 	a8a8a8
                                    gray67 	ababab
                                    gray68 	adadad
                                    gray69 	b0b0b0
                                    gray70 	b3b3b3
                                    gray71 	b5b5b5
                                    gray72 	b8b8b8
                                    gray73 	bababa
                                    gray74 	bdbdbd
                                    gray75 	bfbfbf
                                    gray76 	c2c2c2
                                    gray77 	c4c4c4
                                    gray78 	c7c7c7
                                    gray79 	c9c9c9
                                    gray80 	cccccc
                                    gray81 	cfcfcf
                                    gray82 	d1d1d1
                                    gray83 	d4d4d4
                                    gray84 	d6d6d6
                                    gray85 	d9d9d9
                                    gray86 	dbdbdb
                                    gray87 	dedede
                                    gray88 	e0e0e0
                                    gray89 	e3e3e3
                                    gray90 	e5e5e5
                                    gray91 	e8e8e8
                                    gray92 	ebebeb
                                    gray93 	ededed
                                    gray94 	f0f0f0
                                    gray95 	f2f2f2
                                    gray97 	f7f7f7
                                    gray98 	fafafa
                                    gray99 	fcfcfc
                                    green1 	00ff00
                                    green2 	00ee00
                                    green3 	00cd00
                                    green4 	008b00
                                    greenyellow 	adff2f
                                    honeydew1 	f0fff0
                                    honeydew2 	e0eee0
                                    honeydew3 	c1cdc1
                                    honeydew4 	838b83
                                    hotpink 	ff69b4
                                    hotpink1 	ff6eb4
                                    hotpink2 	ee6aa7
                                    hotpink3 	cd6090
                                    hotpink4 	8b3a62
                                    indianred 	cd5c5c
                                    indianred1 	ff6a6a
                                    indianred2 	ee6363
                                    indianred3 	cd5555
                                    indianred4 	8b3a3a
                                    ivory1 	fffff0
                                    ivory2 	eeeee0
                                    ivory3 	cdcdc1
                                    ivory4 	8b8b83
                                    khaki 	f0e68c
                                    khaki1 	fff68f
                                    khaki2 	eee685
                                    khaki3 	cdc673
                                    khaki4 	8b864e
                                    lavender 	e6e6fa
                                    lavenderblush1 	fff0f5
                                    lavenderblush2 	eee0e5
                                    lavenderblush3 	cdc1c5
                                    lavenderblush4 	8b8386
                                    lawngreen 	7cfc00
                                    lemonchiffon1 	fffacd
                                    lemonchiffon2 	eee9bf
                                    lemonchiffon3 	cdc9a5
                                    lemonchiffon4 	8b8970
                                    light 	eedd82
                                    lightblue 	add8e6
                                    lightblue1 	bfefff
                                    lightblue2 	b2dfee
                                    lightblue3 	9ac0cd
                                    lightblue4 	68838b
                                    lightcoral 	f08080
                                    lightcyan1 	e0ffff
                                    lightcyan2 	d1eeee
                                    lightcyan3 	b4cdcd
                                    lightcyan4 	7a8b8b
                                    lightgoldenrod1 	ffec8b
                                    lightgoldenrod2 	eedc82
                                    lightgoldenrod3 	cdbe70
                                    lightgoldenrod4 	8b814c
                                    lightgoldenrodyellow 	fafad2
                                    lightgray 	d3d3d3
                                    lightpink 	ffb6c1
                                    lightpink1 	ffaeb9
                                    lightpink2 	eea2ad
                                    lightpink3 	cd8c95
                                    lightpink4 	8b5f65
                                    lightsalmon1 	ffa07a
                                    lightsalmon2 	ee9572
                                    lightsalmon3 	cd8162
                                    lightsalmon4 	8b5742
                                    lightseagreen 	20b2aa
                                    lightskyblue 	87cefa
                                    lightskyblue1 	b0e2ff
                                    lightskyblue2 	a4d3ee
                                    lightskyblue3 	8db6cd
                                    lightskyblue4 	607b8b
                                    lightslateblue 	8470ff
                                    lightslategray 	"778899"
                                    lightsteelblue 	b0c4de
                                    lightsteelblue1 	cae1ff
                                    lightsteelblue2 	bcd2ee
                                    lightsteelblue3 	a2b5cd
                                    lightsteelblue4 	6e7b8b
                                    lightyellow1 	ffffe0
                                    lightyellow2 	eeeed1
                                    lightyellow3 	cdcdb4
                                    lightyellow4 	8b8b7a
                                    limegreen 	32cd32
                                    linen 	faf0e6
                                    magenta 	ff00ff
                                    magenta2 	ee00ee
                                    magenta3 	cd00cd
                                    magenta4 	8b008b
                                    maroon 	b03060
                                    maroon1 	ff34b3
                                    maroon2 	ee30a7
                                    maroon3 	cd2990
                                    maroon4 	8b1c62
                                    medium 	66cdaa
                                    mediumaquamarine 	66cdaa
                                    mediumblue 	0000cd
                                    mediumorchid 	ba55d3
                                    mediumorchid1 	e066ff
                                    mediumorchid2 	d15fee
                                    mediumorchid3 	b452cd
                                    mediumorchid4 	7a378b
                                    mediumpurple 	9370db
                                    mediumpurple1 	ab82ff
                                    mediumpurple2 	9f79ee
                                    mediumpurple3 	8968cd
                                    mediumpurple4 	5d478b
                                    mediumseagreen 	3cb371
                                    mediumslateblue 	7b68ee
                                    mediumspringgreen 	00fa9a
                                    mediumturquoise 	48d1cc
                                    mediumvioletred 	c71585
                                    midnightblue 	"191970"
                                    mintcream 	f5fffa
                                    mistyrose1 	ffe4e1
                                    mistyrose2 	eed5d2
                                    mistyrose3 	cdb7b5
                                    mistyrose4 	8b7d7b
                                    moccasin 	ffe4b5
                                    navajowhite1 	ffdead
                                    navajowhite2 	eecfa1
                                    navajowhite3 	cdb38b
                                    navajowhite4 	8b795e
                                    navyblue 	"000080"
                                    oldlace 	fdf5e6
                                    olivedrab 	6b8e23
                                    olivedrab1 	c0ff3e
                                    olivedrab2 	b3ee3a
                                    olivedrab4 	698b22
                                    orange1 	ffa500
                                    orange2 	ee9a00
                                    orange3 	cd8500
                                    orange4 	8b5a00
                                    orangered1 	ff4500
                                    orangered2 	ee4000
                                    orangered3 	cd3700
                                    orangered4 	8b2500
                                    orchid 	da70d6
                                    orchid1 	ff83fa
                                    orchid2 	ee7ae9
                                    orchid3 	cd69c9
                                    orchid4 	8b4789
                                    pale 	db7093
                                    palegoldenrod 	eee8aa
                                    palegreen 	98fb98
                                    palegreen1 	9aff9a
                                    palegreen2 	90ee90
                                    palegreen3 	7ccd7c
                                    palegreen4 	548b54
                                    paleturquoise 	afeeee
                                    paleturquoise1 	bbffff
                                    paleturquoise2 	aeeeee
                                    paleturquoise3 	96cdcd
                                    paleturquoise4 	668b8b
                                    palevioletred 	db7093
                                    palevioletred1 	ff82ab
                                    palevioletred2 	ee799f
                                    palevioletred3 	cd6889
                                    palevioletred4 	8b475d
                                    papayawhip 	ffefd5
                                    peachpuff1 	ffdab9
                                    peachpuff2 	eecbad
                                    peachpuff3 	cdaf95
                                    peachpuff4 	8b7765
                                    pink 	ffc0cb
                                    pink1 	ffb5c5
                                    pink2 	eea9b8
                                    pink3 	cd919e
                                    pink4 	8b636c
                                    plum 	dda0dd
                                    plum1 	ffbbff
                                    plum2 	eeaeee
                                    plum3 	cd96cd
                                    plum4 	8b668b
                                    powderblue 	b0e0e6
                                    purple 	a020f0
                                    purple1 	9b30ff
                                    purple2 	912cee
                                    purple3 	7d26cd
                                    purple4 	551a8b
                                    red1 	ff0000
                                    red2 	ee0000
                                    red3 	cd0000
                                    red4 	8b0000
                                    rosybrown 	bc8f8f
                                    rosybrown1 	ffc1c1
                                    rosybrown2 	eeb4b4
                                    rosybrown3 	cd9b9b
                                    rosybrown4 	8b6969
                                    royalblue 	"4169e1"
                                    royalblue1 	4876ff
                                    royalblue2 	436eee
                                    royalblue3 	3a5fcd
                                    royalblue4 	27408b
                                    saddlebrown 	8b4513
                                    salmon 	fa8072
                                    salmon1 	ff8c69
                                    salmon2 	ee8262
                                    salmon3 	cd7054
                                    salmon4 	8b4c39
                                    sandybrown 	f4a460
                                    seagreen1 	54ff9f
                                    seagreen2 	4eee94
                                    seagreen3 	43cd80
                                    seagreen4 	2e8b57
                                    seashell1 	fff5ee
                                    seashell2 	eee5de
                                    seashell3 	cdc5bf
                                    seashell4 	8b8682
                                    sienna 	a0522d
                                    sienna1 	ff8247
                                    sienna2 	ee7942
                                    sienna3 	cd6839
                                    sienna4 	8b4726
                                    skyblue 	87ceeb
                                    skyblue1 	87ceff
                                    skyblue2 	7ec0ee
                                    skyblue3 	6ca6cd
                                    skyblue4 	4a708b
                                    slateblue 	6a5acd
                                    slateblue1 	836fff
                                    slateblue2 	7a67ee
                                    slateblue3 	6959cd
                                    slateblue4 	473c8b
                                    slategray 	"708090"
                                    slategray1 	c6e2ff
                                    slategray2 	b9d3ee
                                    slategray3 	9fb6cd
                                    slategray4 	6c7b8b
                                    snow1 	fffafa
                                    snow2 	eee9e9
                                    snow3 	cdc9c9
                                    snow4 	8b8989
                                    springgreen1 	00ff7f
                                    springgreen2 	00ee76
                                    springgreen3 	00cd66
                                    springgreen4 	008b45
                                    steelblue 	4682b4
                                    steelblue1 	63b8ff
                                    steelblue2 	5cacee
                                    steelblue3 	4f94cd
                                    steelblue4 	36648b
                                    tan 	d2b48c
                                    tan1 	ffa54f
                                    tan2 	ee9a49
                                    tan3 	cd853f
                                    tan4 	8b5a2b
                                    thistle 	d8bfd8
                                    thistle1 	ffe1ff
                                    thistle2 	eed2ee
                                    thistle3 	cdb5cd
                                    thistle4 	8b7b8b
                                    tomato1 	ff6347
                                    tomato2 	ee5c42
                                    tomato3 	cd4f39
                                    tomato4 	8b3626
                                    turquoise 	40e0d0
                                    turquoise1 	00f5ff
                                    turquoise2 	00e5ee
                                    turquoise3 	00c5cd
                                    turquoise4 	00868b
                                    violet 	ee82ee
                                    violetred 	d02090
                                    violetred1 	ff3e96
                                    violetred2 	ee3a8c
                                    violetred3 	cd3278
                                    violetred4 	8b2252
                                    wheat 	f5deb3
                                    wheat1 	ffe7ba
                                    wheat2 	eed8ae
                                    wheat3 	cdba96
                                    wheat4 	8b7e66
                                    white 	ffffff
                                    whitesmoke 	f5f5f5
                                    yellow1 	ffff00
                                    yellow2 	eeee00
                                    yellow3 	cdcd00
                                    yellow4 	8b8b00
                                    yellowgreen 	9acd32
                                    
                                    aqua  00FFFF

                                    aquamarine  7FFFD4

                                    aquamarine3  66CDAA

                                    azure  F0FFFF

                                    bisque  FFE4C4

                                    blue  0000FF

                                    blue3  0000CD

                                    chartreuse  7FFF00

                                    chocolate4  5C4033

                                    cornsilk  FFF8DC

                                    crimson  DC143C

                                    cyan  00FFFF

                                    darkblue  00008B

                                    darkcyan  008B8B

                                    darkgray  A9A9A9

                                    darkgrey  A9A9A9

                                    darkmagenta  8B008B

                                    darkred  8B0000

                                    darkslategrey  2F4F4F

                                    deeppink  FF1493

                                    deepskyblue  00BFFF

                                    dimgrey  "696969"

                                    dodgerblue  1E90FF

                                    fuchsia  FF00FF

                                    gold  FFD700

                                    gray0  "000000"

                                    gray100  F5F5F5

                                    gray96  F4F4F4

                                    green  "008000"

                                    grey  "808080"

                                    grey0  "000000"

                                    grey1  "030303"

                                    grey10  1A1A1A

                                    grey100  F5F5F5

                                    grey11  1C1C1C

                                    grey12  1F1F1F

                                    grey13  "212121"

                                    grey14  "242424"

                                    grey15 "262626"

                                    grey16  "292929"

                                    grey17  2B2B2B

                                    grey18  2E2E2E

                                    grey19  "303030"

                                    grey2  1A1A1A

                                    grey20  "333333"

                                    grey21  "363636"

                                    grey22  "383838"

                                    grey23  3B3B3B

                                    grey24  3D3D3D

                                    grey25  "404040"

                                    grey26  "424242"

                                    grey27  "454545"

                                    grey28 "474747"

                                    grey29  4A4A4A

                                    grey3  1F1F1F

                                    grey30  4D4D4D

                                    grey31  4F4F4F

                                    grey32  "525252"

                                    grey33  "545454"

                                    grey34  "575757"

                                    grey35  "595959"

                                    grey36  5C5C5C

                                    grey37  5E5E5E

                                    grey38  "616161"

                                    grey39  "636363"

                                    grey4  "212121"

                                    grey40  "666666"

                                    grey41  "686868"

                                    grey42  6B6B6B

                                    grey43  6E6E6E

                                    grey44  "707070"

                                    grey45  "737373"

                                    grey46  "757575"

                                    grey47  "787878"

                                    grey48  7A7A7A

                                    grey49  7D7D7D

                                    grey5  "262626"

                                    grey50  "808080"

                                    grey51  "828282"

                                    grey52  "858585"

                                    grey53  "878787"

                                    grey54  8A8A8A

                                    grey55  8C8C8C

                                    grey56  8F8F8F

                                    grey57  "919191"

                                    grey58  "949494"

                                    grey59  "969696"

                                    grey6  "292929"

                                    grey60  "999999"

                                    grey61  9C9C9C

                                    grey62  9E9E9E

                                    grey63  A1A1A1

                                    grey64  A3A3A3

                                    grey65  A6A6A6

                                    grey66  A8A8A8

                                    grey67  ABABAB

                                    grey68  ADADAD

                                    grey69  B0B0B0

                                    grey7  2E2E2E

                                    grey70  B3B3B3

                                    grey71  B5B5B5

                                    grey72  B8B8B8

                                    grey73  BABABA

                                    grey74  BDBDBD

                                    grey75  BFBFBF

                                    grey76  C2C2C2

                                    grey77  C4C4C4

                                    grey78  C7C7C7

                                    grey79  C9C9C9

                                    grey8  "313131"

                                    grey80  CCCCCC

                                    grey81  CFCFCF

                                    grey82  D1D1D1

                                    grey83  D4D4D4

                                    grey84  D6D6D6

                                    grey85  D9D9D9

                                    grey86  DBDBDB

                                    grey87  DEDEDE

                                    grey88  E0E0E0

                                    grey89  E3E3E3

                                    grey9  "343434"

                                    grey90  E5E5E5

                                    grey91  E8E8E8

                                    grey92  EAEAEA

                                    grey93  ECECEC

                                    grey94  EFEFEF

                                    grey95  F1F1F1

                                    grey96  F4F4F4

                                    grey97  F6F6F6

                                    grey98  F9F9F9

                                    grey99  FCFCFC

                                    honeydew  F0FFF0

                                    indigo  4B0082

                                    invis  "000000"

                                    ivory  FFFFF0

                                    lavenderblush  FFF0F5

                                    lemonchiffon  FFFACD

                                    lightcyan  E0FFFF

                                    lightgoldenrod  FAFAD2

                                    lightgreen  90EE90

                                    lightgrey  D3D3D3

                                    lightsalmon  FFA07A

                                    lightslategrey  "778899"

                                    lightyellow  FFFFE0

                                    lime  00FF00

                                    magenta1  FF00FF

                                    mistyrose  FFE4E1

                                    navajowhite  FFDEAD

                                    navy  "000080"

                                    none  "000000"

                                    olive  "808000"

                                    olivedrab3  6B8E23

                                    orange  FFA500

                                    orangered  FF4500

                                    peachpuff  FFDAB9

                                    peru  CD853F

                                    rebeccapurple  "663399"

                                    red  FF0000

                                    seagreen  2E8B57

                                    seashell  FFF5EE

                                    silver  C0C0C0

                                    slategrey  "708090"

                                    snow  FFFAFA

                                    springgreen  00FF7F

                                    teal  "008080"

                                    tomato  FF6347

                                    transparent  "000000"

                                    webgray  "808080"

                                    webgreen  00FF00

                                    webgrey  "808080"

                                    webmaroon  "800000"

                                    webpurple  "800080"

                                    x11gray  "808080"

                                    x11green  "006400"

                                    x11grey  a9a9a9

                                    x11maroon  b03060

                                    x11purple  a020f0

                                    yellow  FFFF00))

(define (make-color-hex-hash color-list ht)
  (if (null? color-list)
      ht
      (make-color-hex-hash (cdr (cdr color-list))
                           (hash-set ht (car color-list) (if (symbol? (cadr color-list))
                                                               (symbol->string (cadr color-list))
                                                               (cadr color-list))))))

(define hex-codes  (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))

(define hex-values (for/hash
                       ([hex-code (in-list hex-codes)]
                        [val (in-naturals)])
                     (values hex-code val)))


(define (hex->rgb hex-val)
  (let ([digit-one (substring hex-val 0 1)]
        [digit-two (substring hex-val 1)])
    (+ (* (hash-ref hex-values digit-one) 16) (hash-ref hex-values digit-two))))



(define (hex->racketcolor hex-code)
  (let ([red-hex (substring hex-code 0 2)]
        [green-hex (substring hex-code 2 4)]
        [blue-hex (substring hex-code 4 6)])
    (color (hex->rgb red-hex) (hex->rgb green-hex) (hex->rgb blue-hex))))


(define X11-COLOR-HASH (make-color-hex-hash color-hex-list (hash)))

(define X11-AS-RACKET-HASH (hash-map/copy X11-COLOR-HASH
                                          (λ (key value)
                                            (values key (hex->racketcolor (string-upcase value))))))

