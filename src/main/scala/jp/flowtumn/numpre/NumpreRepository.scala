package jp.flowtumn.numpre

/**
 * 解法の内容を記しRepository.
 */
trait NumpreRepository {
	/**
	 * RepositoryにElementを追加します。
	 */
	def push(v: NumpreElement): Boolean

	/**
	 * RepositoryからElementを削除します。
	 */
	def pop: Option[NumpreElement]

	/**
	 * Repositoryに追加された一覧を返します。
	 */
	def toIterable: Iterable[NumpreElement]

	/**
	 * 最後に追加されたElementを返します。
	 */
	def tail: Option[NumpreElement]

	/**
	 * 過去にRepositoryに記録された事があるかどうかを返します。
	 */
	def isPushd(v: NumpreElement): Boolean
}

/**
 * メモリ上に記憶するRepositoryを生成.
 */
object OnMemoryRepositoryFactory {
	def create(v: Iterable[NumpreElement]): NumpreRepository = {
		val result = new NumpreRepository {
			protected val collect = scala.collection.mutable.Stack[NumpreElement]()
			protected val history = scala.collection.mutable.HashMap[Long, Boolean]()

			override def push(v: NumpreElement): Boolean = {
				//todo: 同一データは排除すべき。
				collect.push(v)

				//追加した履歴に残す。
				history(v.hash) = true
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

			override def tail: Option[NumpreElement] = {
				if (0 < collect.size) {
					Some(collect.top)
				} else {
					None
				}
			}

			override def isPushd(v: NumpreElement): Boolean = {
				history.get(v.hash) match {
					case Some(v) =>
						v
					case None =>
						false
				}
			}
		}

		v.foreach(result.push)
		result
	}
}
