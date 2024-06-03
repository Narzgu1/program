package patmat

abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

trait Huffman extends HuffmanInterface:


  def weight(tree: CodeTree): Int =
      tree match{
        case f: Fork => f.weight
        case l: Leaf => l.weight
      }

  def chars(tree: CodeTree): List[Char] =
      tree match {
        case Fork(_, _, chars, _) => chars
        case Leaf(char, _) => List(char)
      }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] =
      chars.groupBy(identity).map {case (c, l) => (c, l.length)}.toList

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
      freqs.sortBy(_._2).map(p => Leaf(p._1, p._2))
    }

  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = {
      if (singleton(trees)) trees
      else {
        val h1 = trees.head
        val h2 = trees.tail.head
        val t = trees.tail.tail
        val f = makeCodeTree(h1, h2)
        (f :: t).sortWith((t1, t2) => weight(t1) < weight(t2))
      }
    }
    
  def until(singletonFn: List[CodeTree] => Boolean,
              combineFn: List[CodeTree] => List[CodeTree])
             (trees: List[CodeTree]): List[CodeTree] = {
      if (singletonFn(trees)) trees else until(singletonFn, combineFn)(trees)
    }


  def createCodeTree(chars: List[Char]): CodeTree =
      until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
      def treeTraverse(remainTree: CodeTree, remainBits: List[Bit]): List[Char] = {
        remainTree match {
          case Leaf(c, _) => if (remainBits.isEmpty) List(c) else c :: treeTraverse(tree, remainBits)
          case Fork(l, r, _, _) =>
            if (remainBits.head == 0) treeTraverse(l, remainBits.tail)
            else treeTraverse(r, remainBits.tail)
        }
      }
      treeTraverse(tree, bits)
    }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
      def charLookup(curTree: CodeTree)(char: Char): List[Bit] =
        curTree match{
          case _: Leaf => List()
          case Fork(l, r, _, _) =>
            if (chars(l).contains(char)) 0 :: charLookup(l)(char)
            else 1 :: charLookup(r)(char)
        }
      text.flatMap(c => charLookup(tree)(c))
    }

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
      table.apply(table.indexWhere(c => c._1 == char))._2
    }

  def convert(tree: CodeTree): CodeTable = tree match {
      case Leaf(c, _) => List((c, List()))
      case Fork(l, r, _, _) => mergeCodeTables(convert(l), convert(r))
    }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
      def prepend(bit: Bit)(charCodeInTable: (Char, List[Bit])): (Char, List[Bit])=
        (charCodeInTable._1, bit :: charCodeInTable._2)
      a.map(prepend(bit = 0)) :::  b.map(prepend(bit = 1))
    }

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
      val table = convert(tree)
      text flatMap codeBits(table)
    }

object Huffman extends Huffman
