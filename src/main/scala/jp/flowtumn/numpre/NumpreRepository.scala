package jp.flowtumn.numpre

/**
 * 解法の内容を記しRepository.
 */
trait NumpreRepository {
    def push(v: NumpreElement): Boolean
    def pop: Option[NumpreElement]
    def toIterable: Iterable[NumpreElement]
}

/**
 *メモリ上に記憶するRepositoryを生成.
 */
object OnMemoryRepositoryFactory {
    def create(v: Iterable[NumpreElement]): NumpreRepository = {
        val result = new NumpreRepository {
            val collect = new scala.collection.mutable.Stack[NumpreElement]()
            
            override def push(v: NumpreElement): Boolean  ={
                //todo: 同一データは排除すべき。
                collect.push(v)
                true
            }

            override def pop: Option[NumpreElement] = {
                if (0 < collect.size) {
                    Some(collect.pop)
                } else {
                    None
                }
            }

            override def toIterable: Iterable[NumpreElement] = {
                collect
            }
        }

        v.foreach(result.push)
        result
    }
}
