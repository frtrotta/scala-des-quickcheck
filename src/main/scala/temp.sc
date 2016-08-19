import quickcheck._

new QuickCheckHeap with BinomialHeap {
  println(genHeap.sample)
}