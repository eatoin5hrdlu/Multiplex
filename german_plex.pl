:-ensure_loaded(plex).
:-no_style_check((discontiguous)).
german([],[]):-!.
german(_19311,_19256):-german(_19311,_19253,_19318),(_19253==' $$ eND oF fILE ## '->_19256=[];_19253=[_19267|_19268]->append(_19253,_19271,_19256);_19253==[]->_19271=_19256;_19256=[_19253|_19271]),german(_19318,_19271).
german([],' $$ eND oF fILE ## ',[]):-!.
german([_19351|_19352],_19355,_19353):-german(_19351,_19352,_19371,_19371,_19369,_19355,_19353).
german_2(1,_19680,[],_19676,prev(_19667,_19668,_19665),_19668,_19667):-_19665.
german_2(2,_19713,[],_19709,prev(_19700,_19701,_19698),_19701,_19700):-_19698.
german_2(3,_19746,[],_19742,prev(_19733,_19734,_19731),_19734,_19733):-_19731.
german_2(4,_19779,[],_19775,prev(_19766,_19767,_19764),_19767,_19766):-_19764.
german_2(5,_19812,[],_19808,prev(_19799,_19800,_19797),_19800,_19799):-_19797.
german_2(6,_19845,[],_19841,prev(_19832,_19833,_19830),_19833,_19832):-_19830.
german_2(7,_19878,[],_19874,prev(_19865,_19866,_19863),_19866,_19865):-_19863.
german_2(8,_19911,[],_19907,prev(_19898,_19899,_19896),_19899,_19898):-_19896.
german_2(9,_19944,[],_19940,prev(_19931,_19932,_19929),_19932,_19931):-_19929.
german_2(10,_19977,[],_19973,prev(_19964,_19965,_19962),_19965,_19964):-_19962.
german_2(11,_20010,[],_20006,prev(_19997,_19998,_19995),_19998,_19997):-_19995.
german_2(12,_20043,[],_20039,prev(_20030,_20031,_20028),_20031,_20030):-_20028.
german_2(13,_20076,[],_20072,prev(_20063,_20064,_20061),_20064,_20063):-_20061.
german_2(14,_20109,[],_20105,prev(_20096,_20097,_20094),_20097,_20096):-_20094.
german_2(15,_20142,[],_20138,prev(_20129,_20130,_20127),_20130,_20129):-_20127.
german_2(16,_20175,[],_20171,prev(_20162,_20163,_20160),_20163,_20162):-_20160.
german_2(17,_20208,[],_20204,prev(_20195,_20196,_20193),_20196,_20195):-_20193.
german_2(18,_20241,[],_20237,prev(_20228,_20229,_20226),_20229,_20228):-_20226.
german_2(19,_20274,[],_20270,prev(_20261,_20262,_20259),_20262,_20261):-_20259.
german_2(20,_20307,[],_20303,prev(_20294,_20295,_20292),_20295,_20294):-_20292.
german_2(21,_20340,[],_20336,prev(_20327,_20328,_20325),_20328,_20327):-_20325.
german_2(22,_20373,[],_20369,prev(_20360,_20361,_20358),_20361,_20360):-_20358.
german_2(23,_20406,[],_20402,prev(_20393,_20394,_20391),_20394,_20393):-_20391.
german_2(24,_20439,[],_20435,prev(_20426,_20427,_20424),_20427,_20426):-_20424.
german_2(25,_20472,[],_20468,prev(_20459,_20460,_20457),_20460,_20459):-_20457.
german_2(26,_20505,[],_20501,prev(_20492,_20493,_20490),_20493,_20492):-_20490.
german_2(27,_20538,[],_20534,prev(_20525,_20526,_20523),_20526,_20525):-_20523.
german_2(28,_20571,[],_20567,prev(_20558,_20559,_20556),_20559,_20558):-_20556.
german_2(29,_20604,[],_20600,prev(_20591,_20592,_20589),_20592,_20591):-_20589.
german_2(30,_20637,[],_20633,prev(_20624,_20625,_20622),_20625,_20624):-_20622.
german_2(31,_20670,[],_20666,prev(_20657,_20658,_20655),_20658,_20657):-_20655.
german_2(32,_20703,[],_20699,prev(_20690,_20691,_20688),_20691,_20690):-_20688.
german_2(33,_20736,[],_20732,prev(_20723,_20724,_20721),_20724,_20723):-_20721.
german_2(34,_20769,[],_20765,prev(_20756,_20757,_20754),_20757,_20756):-_20754.
german_2(35,_20802,[],_20798,prev(_20789,_20790,_20787),_20790,_20789):-_20787.
german_2(36,_20835,[],_20831,prev(_20822,_20823,_20820),_20823,_20822):-_20820.
german_2(37,_20868,[],_20864,prev(_20855,_20856,_20853),_20856,_20855):-_20853.
german_2(38,_20901,[],_20897,prev(_20888,_20889,_20886),_20889,_20888):-_20886.
german_2(39,_20934,[],_20930,prev(_20921,_20922,_20919),_20922,_20921):-_20919.
german_2(40,_20967,[],_20963,prev(_20954,_20955,_20952),_20955,_20954):-_20952.
german_2(41,_21000,[],_20996,prev(_20987,_20988,_20985),_20988,_20987):-_20985.
german_2(42,_21033,[],_21029,prev(_21020,_21021,_21018),_21021,_21020):-_21018.
german_2(43,_21066,[],_21062,prev(_21053,_21054,_21051),_21054,_21053):-_21051.
german_2(44,_21099,[],_21095,prev(_21086,_21087,_21084),_21087,_21086):-_21084.
german_2(45,_21132,[],_21128,prev(_21119,_21120,_21117),_21120,_21119):-_21117.
german_2(46,_21165,[],_21161,prev(_21152,_21153,_21150),_21153,_21152):-_21150.
german_2(47,_21198,[],_21194,prev(_21185,_21186,_21183),_21186,_21185):-_21183.
german_2(48,_21231,[],_21227,prev(_21218,_21219,_21216),_21219,_21218):-_21216.
german_2(49,_21264,[],_21260,prev(_21251,_21252,_21249),_21252,_21251):-_21249.
german_2(50,_21297,[],_21293,prev(_21284,_21285,_21282),_21285,_21284):-_21282.
german_2(51,_21330,[],_21326,prev(_21317,_21318,_21315),_21318,_21317):-_21315.
german_2(52,_21363,[],_21359,prev(_21350,_21351,_21348),_21351,_21350):-_21348.
german_2(53,_21396,[],_21392,prev(_21383,_21384,_21381),_21384,_21383):-_21381.
german_2(54,_21429,[],_21425,prev(_21416,_21417,_21414),_21417,_21416):-_21414.
german_2(55,_21462,[],_21458,prev(_21449,_21450,_21447),_21450,_21449):-_21447.
german_2(56,_21495,[],_21491,prev(_21482,_21483,_21480),_21483,_21482):-_21480.
german_2(57,_21528,[],_21524,prev(_21515,_21516,_21513),_21516,_21515):-_21513.
german_2(58,_21561,[],_21557,prev(_21548,_21549,_21546),_21549,_21548):-_21546.
german_2(59,_21594,[],_21590,prev(_21581,_21582,_21579),_21582,_21581):-_21579.
german_2(60,_21627,[],_21623,prev(_21614,_21615,_21612),_21615,_21614):-_21612.
german_2(61,_21660,[],_21656,prev(_21647,_21648,_21645),_21648,_21647):-_21645.
german_2(62,_21693,[],_21689,prev(_21680,_21681,_21678),_21681,_21680):-_21678.
german_2(63,_21726,[],_21722,prev(_21713,_21714,_21711),_21714,_21713):-_21711.
german_2(64,_21759,[],_21755,prev(_21746,_21747,_21744),_21747,_21746):-_21744.
german_2(65,_21792,[],_21788,prev(_21779,_21780,_21777),_21780,_21779):-_21777.
german_2(66,_21825,[],_21821,prev(_21812,_21813,_21810),_21813,_21812):-_21810.
german_2(67,_21858,[],_21854,prev(_21845,_21846,_21843),_21846,_21845):-_21843.
german_2(68,_21891,[],_21887,prev(_21878,_21879,_21876),_21879,_21878):-_21876.
german_2(69,_21924,[],_21920,prev(_21911,_21912,_21909),_21912,_21911):-_21909.
german_2(70,_21957,[],_21953,prev(_21944,_21945,_21942),_21945,_21944):-_21942.
german_2(71,_21990,[],_21986,prev(_21977,_21978,_21975),_21978,_21977):-_21975.
german_2(72,_22023,[],_22019,prev(_22010,_22011,_22008),_22011,_22010):-_22008.
german_2(73,_22056,[],_22052,prev(_22043,_22044,_22041),_22044,_22043):-_22041.
german_2(74,_22089,[],_22085,prev(_22076,_22077,_22074),_22077,_22076):-_22074.
german_2(75,_22122,[],_22118,prev(_22109,_22110,_22107),_22110,_22109):-_22107.
german_2(76,_22155,[],_22151,prev(_22142,_22143,_22140),_22143,_22142):-_22140.
german_2(77,_22188,[],_22184,prev(_22175,_22176,_22173),_22176,_22175):-_22173.
german_2(78,_22221,[],_22217,prev(_22208,_22209,_22206),_22209,_22208):-_22206.
german_2(79,_22254,[],_22250,prev(_22241,_22242,_22239),_22242,_22241):-_22239.
german_2(80,_22287,[],_22283,prev(_22274,_22275,_22272),_22275,_22274):-_22272.
german_2(81,_22320,[],_22316,prev(_22307,_22308,_22305),_22308,_22307):-_22305.
german_2(82,_22353,[],_22349,prev(_22340,_22341,_22338),_22341,_22340):-_22338.
german_2(83,_22386,[],_22382,prev(_22373,_22374,_22371),_22374,_22373):-_22371.
german_2(84,_22419,[],_22415,prev(_22406,_22407,_22404),_22407,_22406):-_22404.
german_2(85,_22452,[],_22448,prev(_22439,_22440,_22437),_22440,_22439):-_22437.
german_2(86,_22485,[],_22481,prev(_22472,_22473,_22470),_22473,_22472):-_22470.
german_2(87,_22518,[],_22514,prev(_22505,_22506,_22503),_22506,_22505):-_22503.
german_2(88,_22551,[],_22547,prev(_22538,_22539,_22536),_22539,_22538):-_22536.
german_2(89,_22584,[],_22580,prev(_22571,_22572,_22569),_22572,_22571):-_22569.
german_2(90,_22617,[],_22613,prev(_22604,_22605,_22602),_22605,_22604):-_22602.
german_2(91,_22650,[],_22646,prev(_22637,_22638,_22635),_22638,_22637):-_22635.
german_2(92,_22683,[],_22679,prev(_22670,_22671,_22668),_22671,_22670):-_22668.
german_2(93,_22716,[],_22712,prev(_22703,_22704,_22701),_22704,_22703):-_22701.
german_2(94,_22749,[],_22745,prev(_22736,_22737,_22734),_22737,_22736):-_22734.
german_2(95,_22782,[],_22778,prev(_22769,_22770,_22767),_22770,_22769):-_22767.
german_2(96,_22815,[],_22811,prev(_22802,_22803,_22800),_22803,_22802):-_22800.
german_2(97,_22848,[],_22844,prev(_22835,_22836,_22833),_22836,_22835):-_22833.
german_2(98,_22881,[],_22877,prev(_22868,_22869,_22866),_22869,_22868):-_22866.
german_2(99,_22914,[],_22910,prev(_22901,_22902,_22899),_22902,_22901):-_22899.
german_2(100,_22947,[],_22943,prev(_22934,_22935,_22932),_22935,_22934):-_22932.
german_2(101,_22980,[],_22976,prev(_22967,_22968,_22965),_22968,_22967):-_22965.
german_2(102,_23013,[],_23009,prev(_23000,_23001,_22998),_23001,_23000):-_22998.
german_2(103,_23046,[],_23042,prev(_23033,_23034,_23031),_23034,_23033):-_23031.
german_2(104,_23079,[],_23075,prev(_23066,_23067,_23064),_23067,_23066):-_23064.
german_2(105,_23112,[],_23108,prev(_23099,_23100,_23097),_23100,_23099):-_23097.
german_2(106,_23145,[],_23141,prev(_23132,_23133,_23130),_23133,_23132):-_23130.
german_2(107,_23178,[],_23174,prev(_23165,_23166,_23163),_23166,_23165):-_23163.
german_2(108,_23211,[],_23207,prev(_23198,_23199,_23196),_23199,_23198):-_23196.
german_2(109,_23244,[],_23240,prev(_23231,_23232,_23229),_23232,_23231):-_23229.
german_2(110,_23277,[],_23273,prev(_23264,_23265,_23262),_23265,_23264):-_23262.
german_2(111,_23310,[],_23306,prev(_23297,_23298,_23295),_23298,_23297):-_23295.
german_2(112,_23343,[],_23339,prev(_23330,_23331,_23328),_23331,_23330):-_23328.
german_2(113,_23376,[],_23372,prev(_23363,_23364,_23361),_23364,_23363):-_23361.
german_2(114,_23409,[],_23405,prev(_23396,_23397,_23394),_23397,_23396):-_23394.
german_2(115,_23442,[],_23438,prev(_23429,_23430,_23427),_23430,_23429):-_23427.
german_2(116,_23475,[],_23471,prev(_23462,_23463,_23460),_23463,_23462):-_23460.
german_2(117,_23508,[],_23504,prev(_23495,_23496,_23493),_23496,_23495):-_23493.
german_2(118,_23541,[],_23537,prev(_23528,_23529,_23526),_23529,_23528):-_23526.
german_2(119,_23574,[],_23570,prev(_23561,_23562,_23559),_23562,_23561):-_23559.
german_2(120,_23607,[],_23603,prev(_23594,_23595,_23592),_23595,_23594):-_23592.
german_2(121,_23640,[],_23636,prev(_23627,_23628,_23625),_23628,_23627):-_23625.
german_2(122,_23673,[],_23669,prev(_23660,_23661,_23658),_23661,_23660):-_23658.
german_2(123,_23706,[],_23702,prev(_23693,_23694,_23691),_23694,_23693):-_23691.
german_2(124,_23739,[],_23735,prev(_23726,_23727,_23724),_23727,_23726):-_23724.
german_2(125,_23772,[],_23768,prev(_23759,_23760,_23757),_23760,_23759):-_23757.
german_2(126,_23805,[],_23801,prev(_23792,_23793,_23790),_23793,_23792):-_23790.
german_2(127,_23838,[],_23834,prev(_23825,_23826,_23823),_23826,_23825):-_23823.
german_2(128,_23871,[],_23867,prev(_23858,_23859,_23856),_23859,_23858):-_23856.
german_1(65,_24160,[65|_24203],_24210,_24208,_24206,_24204):-_24160==[]->_24203=[],_24210=_24195,_24204=_24160,_24172=_24206,true,name(_24172,_24195);_24160=[_24168|_24169],plex:strcpy(_24210,_24195),german_1(_24168,_24169,_24203,_24210,prev(_24160,_24172,(true,name(_24172,_24195))),_24206,_24204).
german_1(66,_24301,[66|_24344],_24351,_24349,_24347,_24345):-_24301==[]->_24344=[],_24351=_24336,_24345=_24301,_24313=_24347,true,name(_24313,_24336);_24301=[_24309|_24310],plex:strcpy(_24351,_24336),german_1(_24309,_24310,_24344,_24351,prev(_24301,_24313,(true,name(_24313,_24336))),_24347,_24345).
german_1(67,_24442,[67|_24485],_24492,_24490,_24488,_24486):-_24442==[]->_24485=[],_24492=_24477,_24486=_24442,_24454=_24488,true,name(_24454,_24477);_24442=[_24450|_24451],plex:strcpy(_24492,_24477),german_1(_24450,_24451,_24485,_24492,prev(_24442,_24454,(true,name(_24454,_24477))),_24488,_24486).
german_1(1,_24846,[],_24842,prev(_24833,_24834,_24831),_24834,_24833):-_24831.
german_1(2,_24879,[],_24875,prev(_24866,_24867,_24864),_24867,_24866):-_24864.
german_1(3,_24912,[],_24908,prev(_24899,_24900,_24897),_24900,_24899):-_24897.
german_1(4,_24945,[],_24941,prev(_24932,_24933,_24930),_24933,_24932):-_24930.
german_1(5,_24978,[],_24974,prev(_24965,_24966,_24963),_24966,_24965):-_24963.
german_1(6,_25011,[],_25007,prev(_24998,_24999,_24996),_24999,_24998):-_24996.
german_1(7,_25044,[],_25040,prev(_25031,_25032,_25029),_25032,_25031):-_25029.
german_1(8,_25077,[],_25073,prev(_25064,_25065,_25062),_25065,_25064):-_25062.
german_1(9,_25110,[],_25106,prev(_25097,_25098,_25095),_25098,_25097):-_25095.
german_1(10,_25143,[],_25139,prev(_25130,_25131,_25128),_25131,_25130):-_25128.
german_1(11,_25176,[],_25172,prev(_25163,_25164,_25161),_25164,_25163):-_25161.
german_1(12,_25209,[],_25205,prev(_25196,_25197,_25194),_25197,_25196):-_25194.
german_1(13,_25242,[],_25238,prev(_25229,_25230,_25227),_25230,_25229):-_25227.
german_1(14,_25275,[],_25271,prev(_25262,_25263,_25260),_25263,_25262):-_25260.
german_1(15,_25308,[],_25304,prev(_25295,_25296,_25293),_25296,_25295):-_25293.
german_1(16,_25341,[],_25337,prev(_25328,_25329,_25326),_25329,_25328):-_25326.
german_1(17,_25374,[],_25370,prev(_25361,_25362,_25359),_25362,_25361):-_25359.
german_1(18,_25407,[],_25403,prev(_25394,_25395,_25392),_25395,_25394):-_25392.
german_1(19,_25440,[],_25436,prev(_25427,_25428,_25425),_25428,_25427):-_25425.
german_1(20,_25473,[],_25469,prev(_25460,_25461,_25458),_25461,_25460):-_25458.
german_1(21,_25506,[],_25502,prev(_25493,_25494,_25491),_25494,_25493):-_25491.
german_1(22,_25539,[],_25535,prev(_25526,_25527,_25524),_25527,_25526):-_25524.
german_1(23,_25572,[],_25568,prev(_25559,_25560,_25557),_25560,_25559):-_25557.
german_1(24,_25605,[],_25601,prev(_25592,_25593,_25590),_25593,_25592):-_25590.
german_1(25,_25638,[],_25634,prev(_25625,_25626,_25623),_25626,_25625):-_25623.
german_1(26,_25671,[],_25667,prev(_25658,_25659,_25656),_25659,_25658):-_25656.
german_1(27,_25704,[],_25700,prev(_25691,_25692,_25689),_25692,_25691):-_25689.
german_1(28,_25737,[],_25733,prev(_25724,_25725,_25722),_25725,_25724):-_25722.
german_1(29,_25770,[],_25766,prev(_25757,_25758,_25755),_25758,_25757):-_25755.
german_1(30,_25803,[],_25799,prev(_25790,_25791,_25788),_25791,_25790):-_25788.
german_1(31,_25836,[],_25832,prev(_25823,_25824,_25821),_25824,_25823):-_25821.
german_1(32,_25869,[],_25865,prev(_25856,_25857,_25854),_25857,_25856):-_25854.
german_1(33,_25902,[],_25898,prev(_25889,_25890,_25887),_25890,_25889):-_25887.
german_1(34,_25935,[],_25931,prev(_25922,_25923,_25920),_25923,_25922):-_25920.
german_1(35,_25968,[],_25964,prev(_25955,_25956,_25953),_25956,_25955):-_25953.
german_1(36,_26001,[],_25997,prev(_25988,_25989,_25986),_25989,_25988):-_25986.
german_1(37,_26034,[],_26030,prev(_26021,_26022,_26019),_26022,_26021):-_26019.
german_1(38,_26067,[],_26063,prev(_26054,_26055,_26052),_26055,_26054):-_26052.
german_1(39,_26100,[],_26096,prev(_26087,_26088,_26085),_26088,_26087):-_26085.
german_1(40,_26133,[],_26129,prev(_26120,_26121,_26118),_26121,_26120):-_26118.
german_1(41,_26166,[],_26162,prev(_26153,_26154,_26151),_26154,_26153):-_26151.
german_1(42,_26199,[],_26195,prev(_26186,_26187,_26184),_26187,_26186):-_26184.
german_1(43,_26232,[],_26228,prev(_26219,_26220,_26217),_26220,_26219):-_26217.
german_1(44,_26265,[],_26261,prev(_26252,_26253,_26250),_26253,_26252):-_26250.
german_1(45,_26298,[],_26294,prev(_26285,_26286,_26283),_26286,_26285):-_26283.
german_1(46,_26331,[],_26327,prev(_26318,_26319,_26316),_26319,_26318):-_26316.
german_1(47,_26364,[],_26360,prev(_26351,_26352,_26349),_26352,_26351):-_26349.
german_1(48,_26397,[],_26393,prev(_26384,_26385,_26382),_26385,_26384):-_26382.
german_1(49,_26430,[],_26426,prev(_26417,_26418,_26415),_26418,_26417):-_26415.
german_1(50,_26463,[],_26459,prev(_26450,_26451,_26448),_26451,_26450):-_26448.
german_1(51,_26496,[],_26492,prev(_26483,_26484,_26481),_26484,_26483):-_26481.
german_1(52,_26529,[],_26525,prev(_26516,_26517,_26514),_26517,_26516):-_26514.
german_1(53,_26562,[],_26558,prev(_26549,_26550,_26547),_26550,_26549):-_26547.
german_1(54,_26595,[],_26591,prev(_26582,_26583,_26580),_26583,_26582):-_26580.
german_1(55,_26628,[],_26624,prev(_26615,_26616,_26613),_26616,_26615):-_26613.
german_1(56,_26661,[],_26657,prev(_26648,_26649,_26646),_26649,_26648):-_26646.
german_1(57,_26694,[],_26690,prev(_26681,_26682,_26679),_26682,_26681):-_26679.
german_1(58,_26727,[],_26723,prev(_26714,_26715,_26712),_26715,_26714):-_26712.
german_1(59,_26760,[],_26756,prev(_26747,_26748,_26745),_26748,_26747):-_26745.
german_1(60,_26793,[],_26789,prev(_26780,_26781,_26778),_26781,_26780):-_26778.
german_1(61,_26826,[],_26822,prev(_26813,_26814,_26811),_26814,_26813):-_26811.
german_1(62,_26859,[],_26855,prev(_26846,_26847,_26844),_26847,_26846):-_26844.
german_1(63,_26892,[],_26888,prev(_26879,_26880,_26877),_26880,_26879):-_26877.
german_1(64,_26925,[],_26921,prev(_26912,_26913,_26910),_26913,_26912):-_26910.
german_1(68,_26958,[],_26954,prev(_26945,_26946,_26943),_26946,_26945):-_26943.
german_1(69,_26991,[],_26987,prev(_26978,_26979,_26976),_26979,_26978):-_26976.
german_1(70,_27024,[],_27020,prev(_27011,_27012,_27009),_27012,_27011):-_27009.
german_1(71,_27057,[],_27053,prev(_27044,_27045,_27042),_27045,_27044):-_27042.
german_1(72,_27090,[],_27086,prev(_27077,_27078,_27075),_27078,_27077):-_27075.
german_1(73,_27123,[],_27119,prev(_27110,_27111,_27108),_27111,_27110):-_27108.
german_1(74,_27156,[],_27152,prev(_27143,_27144,_27141),_27144,_27143):-_27141.
german_1(75,_27189,[],_27185,prev(_27176,_27177,_27174),_27177,_27176):-_27174.
german_1(76,_27222,[],_27218,prev(_27209,_27210,_27207),_27210,_27209):-_27207.
german_1(77,_27255,[],_27251,prev(_27242,_27243,_27240),_27243,_27242):-_27240.
german_1(78,_27288,[],_27284,prev(_27275,_27276,_27273),_27276,_27275):-_27273.
german_1(79,_27321,[],_27317,prev(_27308,_27309,_27306),_27309,_27308):-_27306.
german_1(80,_27354,[],_27350,prev(_27341,_27342,_27339),_27342,_27341):-_27339.
german_1(81,_27387,[],_27383,prev(_27374,_27375,_27372),_27375,_27374):-_27372.
german_1(82,_27420,[],_27416,prev(_27407,_27408,_27405),_27408,_27407):-_27405.
german_1(83,_27453,[],_27449,prev(_27440,_27441,_27438),_27441,_27440):-_27438.
german_1(84,_27486,[],_27482,prev(_27473,_27474,_27471),_27474,_27473):-_27471.
german_1(85,_27519,[],_27515,prev(_27506,_27507,_27504),_27507,_27506):-_27504.
german_1(86,_27552,[],_27548,prev(_27539,_27540,_27537),_27540,_27539):-_27537.
german_1(87,_27585,[],_27581,prev(_27572,_27573,_27570),_27573,_27572):-_27570.
german_1(88,_27618,[],_27614,prev(_27605,_27606,_27603),_27606,_27605):-_27603.
german_1(89,_27651,[],_27647,prev(_27638,_27639,_27636),_27639,_27638):-_27636.
german_1(90,_27684,[],_27680,prev(_27671,_27672,_27669),_27672,_27671):-_27669.
german_1(91,_27717,[],_27713,prev(_27704,_27705,_27702),_27705,_27704):-_27702.
german_1(92,_27750,[],_27746,prev(_27737,_27738,_27735),_27738,_27737):-_27735.
german_1(93,_27783,[],_27779,prev(_27770,_27771,_27768),_27771,_27770):-_27768.
german_1(94,_27816,[],_27812,prev(_27803,_27804,_27801),_27804,_27803):-_27801.
german_1(95,_27849,[],_27845,prev(_27836,_27837,_27834),_27837,_27836):-_27834.
german_1(96,_27882,[],_27878,prev(_27869,_27870,_27867),_27870,_27869):-_27867.
german_1(97,_27915,[],_27911,prev(_27902,_27903,_27900),_27903,_27902):-_27900.
german_1(98,_27948,[],_27944,prev(_27935,_27936,_27933),_27936,_27935):-_27933.
german_1(99,_27981,[],_27977,prev(_27968,_27969,_27966),_27969,_27968):-_27966.
german_1(100,_28014,[],_28010,prev(_28001,_28002,_27999),_28002,_28001):-_27999.
german_1(101,_28047,[],_28043,prev(_28034,_28035,_28032),_28035,_28034):-_28032.
german_1(102,_28080,[],_28076,prev(_28067,_28068,_28065),_28068,_28067):-_28065.
german_1(103,_28113,[],_28109,prev(_28100,_28101,_28098),_28101,_28100):-_28098.
german_1(104,_28146,[],_28142,prev(_28133,_28134,_28131),_28134,_28133):-_28131.
german_1(105,_28179,[],_28175,prev(_28166,_28167,_28164),_28167,_28166):-_28164.
german_1(106,_28212,[],_28208,prev(_28199,_28200,_28197),_28200,_28199):-_28197.
german_1(107,_28245,[],_28241,prev(_28232,_28233,_28230),_28233,_28232):-_28230.
german_1(108,_28278,[],_28274,prev(_28265,_28266,_28263),_28266,_28265):-_28263.
german_1(109,_28311,[],_28307,prev(_28298,_28299,_28296),_28299,_28298):-_28296.
german_1(110,_28344,[],_28340,prev(_28331,_28332,_28329),_28332,_28331):-_28329.
german_1(111,_28377,[],_28373,prev(_28364,_28365,_28362),_28365,_28364):-_28362.
german_1(112,_28410,[],_28406,prev(_28397,_28398,_28395),_28398,_28397):-_28395.
german_1(113,_28443,[],_28439,prev(_28430,_28431,_28428),_28431,_28430):-_28428.
german_1(114,_28476,[],_28472,prev(_28463,_28464,_28461),_28464,_28463):-_28461.
german_1(115,_28509,[],_28505,prev(_28496,_28497,_28494),_28497,_28496):-_28494.
german_1(116,_28542,[],_28538,prev(_28529,_28530,_28527),_28530,_28529):-_28527.
german_1(117,_28575,[],_28571,prev(_28562,_28563,_28560),_28563,_28562):-_28560.
german_1(118,_28608,[],_28604,prev(_28595,_28596,_28593),_28596,_28595):-_28593.
german_1(119,_28641,[],_28637,prev(_28628,_28629,_28626),_28629,_28628):-_28626.
german_1(120,_28674,[],_28670,prev(_28661,_28662,_28659),_28662,_28661):-_28659.
german_1(121,_28707,[],_28703,prev(_28694,_28695,_28692),_28695,_28694):-_28692.
german_1(122,_28740,[],_28736,prev(_28727,_28728,_28725),_28728,_28727):-_28725.
german_1(123,_28773,[],_28769,prev(_28760,_28761,_28758),_28761,_28760):-_28758.
german_1(124,_28806,[],_28802,prev(_28793,_28794,_28791),_28794,_28793):-_28791.
german_1(125,_28839,[],_28835,prev(_28826,_28827,_28824),_28827,_28826):-_28824.
german_1(126,_28872,[],_28868,prev(_28859,_28860,_28857),_28860,_28859):-_28857.
german_1(127,_28905,[],_28901,prev(_28892,_28893,_28890),_28893,_28892):-_28890.
german_1(128,_28938,[],_28934,prev(_28925,_28926,_28923),_28926,_28925):-_28923.
german(65,_29227,[65|_29270],_29277,_29275,_29273,_29271):-_29227==[]->_29270=[],_29277=_29262,_29271=_29227,_29239=_29273,true,name(_29239,_29262);_29227=[_29235|_29236],plex:strcpy(_29277,_29262),german_1(_29235,_29236,_29270,_29277,prev(_29227,_29239,(true,name(_29239,_29262))),_29273,_29271).
german(66,_29368,[66|_29411],_29418,_29416,_29414,_29412):-_29368==[]->_29411=[],_29418=_29403,_29412=_29368,_29380=_29414,true,name(_29380,_29403);_29368=[_29376|_29377],plex:strcpy(_29418,_29403),german_1(_29376,_29377,_29411,_29418,prev(_29368,_29380,(true,name(_29380,_29403))),_29414,_29412).
german(67,_29509,[67|_29552],_29559,_29557,_29555,_29553):-_29509==[]->_29552=[],_29559=_29544,_29553=_29509,_29521=_29555,true,name(_29521,_29544);_29509=[_29517|_29518],plex:strcpy(_29559,_29544),german_1(_29517,_29518,_29552,_29559,prev(_29509,_29521,(true,name(_29521,_29544))),_29555,_29553).
german(9,_29906,[9|_29950],_29957,_29955,_29953,_29951):-_29906==[]->_29950=[],_29957=_30014,_29951=[],_29918=_29953,german(_29906,_29918);_29906=[_29914|_29915],german_2(_29914,_29915,_29950,_29957,prev([],_29918,german(_29906,_29918)),_29953,_29951).
german(10,_30039,[10|_30083],_30090,_30088,_30086,_30084):-_30039==[]->_30083=[],_30090=_30147,_30084=[],_30051=_30086,german(_30039,_30051);_30039=[_30047|_30048],german_2(_30047,_30048,_30083,_30090,prev([],_30051,german(_30039,_30051)),_30086,_30084).
german(32,_30172,[32|_30216],_30223,_30221,_30219,_30217):-_30172==[]->_30216=[],_30223=_30280,_30217=[],_30184=_30219,german(_30172,_30184);_30172=[_30180|_30181],german_2(_30180,_30181,_30216,_30223,prev([],_30184,german(_30172,_30184)),_30219,_30217).
