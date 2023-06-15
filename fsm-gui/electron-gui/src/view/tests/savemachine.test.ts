import { dfaNdfaToString, pdaToString, tmMttmToString } from '../saveMachine';

test('Save dfa', () => {
  const machine = JSON.parse(
    `{"states":[{"invFunc":null,"name":"S","type":"start"},{"invFunc":null,"name":"A","type":"normal"},{"invFunc":null,"name":"F","type":"final"},{"invFunc":null,"name":"D","type":"normal"}],"rules":[{"end":"F","input":"a","start":"S"},{"end":"F","input":"a","start":"F"},{"end":"A","input":"b","start":"S"},{"end":"F","input":"a","start":"A"},{"end":"A","input":"b","start":"A"},{"end":"D","input":"b","start":"F"},{"end":"D","input":"a","start":"D"},{"end":"D","input":"b","start":"D"}],"alphabet":["a","b"],"stackAlpha":[],"input":[],"transitions":{"transitions":[],"index":-1,"inputIndex":-1},"type":"dfa","nodead":true,"initialTapePosition":0,"graphVizImage":"/var/tmp/vizTool_electron1.svg"}`,
  );
  const actual = dfaNdfaToString(machine);

  const expected =
    "(make-dfa\n\t'(S A F D)\n\t'(a b)\n\t'S\n\t'(F)\n\t'((S a F) (F a F) (S b A) (A a F) (A b A) (F b D) (D a D) (D b D))\n\t'no-dead)";

  expect(actual).toBe(expected);
});

test('Save pda', () => {
  const machine = JSON.parse(
    `{"states":[{"invFunc":null,"name":"S","type":"start"},{"invFunc":null,"name":"M1","type":"normal"},{"invFunc":null,"name":"F","type":"final"}],"rules":[{"end":"M1","input":"ε","popped":[],"pushed":[],"start":"S"},{"end":"M1","input":"a","popped":[],"pushed":["a","a"],"start":"M1"},{"end":"M1","input":"b","popped":[],"pushed":["b"],"start":"M1"},{"end":"M1","input":"a","popped":["b"],"pushed":["a"],"start":"M1"},{"end":"M1","input":"a","popped":["b","b"],"pushed":[],"start":"M1"},{"end":"M1","input":"b","popped":["a"],"pushed":[],"start":"M1"},{"end":"F","input":"ε","popped":[],"pushed":[],"start":"M1"}],"alphabet":["a","b"],"stackAlpha":["a","b"],"input":[],"transitions":{"transitions":[],"index":-1,"inputIndex":-1},"type":"pda","nodead":true,"initialTapePosition":0,"graphVizImage":"/var/tmp/vizTool_electron1.svg"}`,
  );
  const actual = pdaToString(machine);

  const expected =
    "(make-pda\n\t'(S M1 F)\n\t'(a b)\n\t'(a b)\n\t'S\n\t'(F)\n\t'(((S ε ε) (M1 ε)) ((M1 a ε) (M1 (a a))) ((M1 b ε) (M1 (b))) ((M1 a (b)) (M1 (a))) ((M1 a (b b)) (M1 ε)) ((M1 b (a)) (M1 ε)) ((M1 ε ε) (F ε))))";

  expect(actual).toBe(expected);
});

test('Save tm', () => {
  const machine = JSON.parse(
    `{"states":[{"invFunc":null,"name":"S","type":"start"},{"invFunc":null,"name":"B","type":"normal"},{"invFunc":null,"name":"C","type":"normal"},{"invFunc":null,"name":"D","type":"normal"},{"invFunc":null,"name":"E","type":"normal"},{"invFunc":null,"name":"Y","type":"accept"},{"invFunc":null,"name":"N","type":"final"}],"rules":[{"end":"S","endTape":"R","start":"S","startTape":"@"},{"end":"B","endTape":"R","start":"B","startTape":"@"},{"end":"C","endTape":"R","start":"C","startTape":"@"},{"end":"D","endTape":"R","start":"D","startTape":"@"},{"end":"E","endTape":"R","start":"E","startTape":"@"},{"end":"B","endTape":["z"],"start":"S","startTape":["a"]},{"end":"N","endTape":["b"],"start":"S","startTape":["b"]},{"end":"N","endTape":["c"],"start":"S","startTape":["c"]},{"end":"Y","endTape":"_","start":"S","startTape":"_"},{"end":"N","endTape":["z"],"start":"S","startTape":["z"]},{"end":"N","endTape":["x"],"start":"S","startTape":["x"]},{"end":"N","endTape":["y"],"start":"S","startTape":["y"]},{"end":"E","endTape":"R","start":"E","startTape":["z"]},{"end":"E","endTape":"R","start":"E","startTape":["x"]},{"end":"E","endTape":"R","start":"E","startTape":["y"]},{"end":"Y","endTape":"_","start":"E","startTape":"_"},{"end":"N","endTape":["a"],"start":"E","startTape":["a"]},{"end":"N","endTape":["b"],"start":"E","startTape":["b"]},{"end":"N","endTape":["c"],"start":"E","startTape":["c"]},{"end":"B","endTape":"R","start":"B","startTape":["a"]},{"end":"C","endTape":["x"],"start":"B","startTape":["b"]},{"end":"N","endTape":["c"],"start":"B","startTape":["c"]},{"end":"N","endTape":"_","start":"B","startTape":"_"},{"end":"B","endTape":"R","start":"B","startTape":["z"]},{"end":"B","endTape":"R","start":"B","startTape":["x"]},{"end":"B","endTape":"R","start":"B","startTape":["y"]},{"end":"N","endTape":["a"],"start":"C","startTape":["a"]},{"end":"C","endTape":"R","start":"C","startTape":["b"]},{"end":"D","endTape":["y"],"start":"C","startTape":["c"]},{"end":"N","endTape":"_","start":"C","startTape":"_"},{"end":"C","endTape":"R","start":"C","startTape":["z"]},{"end":"C","endTape":"R","start":"C","startTape":["x"]},{"end":"C","endTape":"R","start":"C","startTape":["y"]},{"end":"S","endTape":["a"],"start":"D","startTape":["a"]},{"end":"D","endTape":"L","start":"D","startTape":["b"]},{"end":"D","endTape":"L","start":"D","startTape":["c"]},{"end":"N","endTape":"_","start":"D","startTape":"_"},{"end":"D","endTape":"L","start":"D","startTape":["z"]},{"end":"D","endTape":"L","start":"D","startTape":["x"]},{"end":"D","endTape":"L","start":"D","startTape":["y"]},{"end":"E","endTape":"R","start":"D","startTape":"@"}],"alphabet":["@","a","b","c","z","x","y"],"stackAlpha":[],"input":["@"],"transitions":{"transitions":[],"index":-1,"inputIndex":-1},"type":"tm-language-recognizer","nodead":true,"accept":{"invFunc":null,"name":"Y","type":"accept"},"initialTapePosition":0,"graphVizImage":"/var/tmp/vizTool_electron1.svg"}`,
  );
  const actual = tmMttmToString(machine);

  const expected =
    "(make-tm\n\t'(S B C D E Y N)\n\t'(@ a b c z x y)\n\t'(((S @) (S R)) ((B @) (B R)) ((C @) (C R)) ((D @) (D R)) ((E @) (E R)) ((S a) (B z)) ((S b) (N b)) ((S c) (N c)) ((S _) (Y _)) ((S z) (N z)) ((S x) (N x)) ((S y) (N y)) ((E z) (E R)) ((E x) (E R)) ((E y) (E R)) ((E _) (Y _)) ((E a) (N a)) ((E b) (N b)) ((E c) (N c)) ((B a) (B R)) ((B b) (C x)) ((B c) (N c)) ((B _) (N _)) ((B z) (B R)) ((B x) (B R)) ((B y) (B R)) ((C a) (N a)) ((C b) (C R)) ((C c) (D y)) ((C _) (N _)) ((C z) (C R)) ((C x) (C R)) ((C y) (C R)) ((D a) (S a)) ((D b) (D L)) ((D c) (D L)) ((D _) (N _)) ((D z) (D L)) ((D x) (D L)) ((D y) (D L)) ((D @) (E R)))\n\t'S\n\t'(Y N)\n\t'Y)";

  expect(actual).toBe(expected);
});
