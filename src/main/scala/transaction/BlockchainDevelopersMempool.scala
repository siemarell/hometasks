package transaction

import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool

import scala.util.{Failure, Success, Try}

class BlockchainDevelopersMempool(transactionsMap: Map[ModifierId, BlockchainDevelopersTransaction]) extends MemoryPool[BlockchainDevelopersTransaction, BlockchainDevelopersMempool] {

  override def put(tx: BlockchainDevelopersTransaction): Try[BlockchainDevelopersMempool] = {
    if (contains(tx.id)) {
      Failure(new Exception(s"Mempool already contains transanction with id ${tx.id}"))
    } else {
      val newTxs = transactionsMap + (tx.id -> tx)
      Success(new BlockchainDevelopersMempool(newTxs))
    }
  }

  override def put(txs: Iterable[BlockchainDevelopersTransaction]): Try[BlockchainDevelopersMempool] = Try(putWithoutCheck(txs))


  override def putWithoutCheck(txs: Iterable[BlockchainDevelopersTransaction]): BlockchainDevelopersMempool =
    txs.foldLeft(this)((inter,tx)=> inter.put(tx) match {
      case Success(mempool) => mempool
      case Failure(e) => throw new Exception(e)
    })

  override def remove(tx: BlockchainDevelopersTransaction): BlockchainDevelopersMempool = new BlockchainDevelopersMempool(transactionsMap - tx.id)

  override def filter(condition: BlockchainDevelopersTransaction => Boolean): BlockchainDevelopersMempool = new BlockchainDevelopersMempool(
    this.transactionsMap.filter(kv => condition(kv._2))
  )

  override def getById(id: ModifierId): Option[BlockchainDevelopersTransaction] = transactionsMap.get(id)

  override def contains(id: ModifierId): Boolean = transactionsMap.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[BlockchainDevelopersTransaction] = transactionsMap.filter(kv=> ids.contains(kv._1)).values.toIndexedSeq

  override def size: Int = transactionsMap.size

  override def take(limit: Int): Iterable[BlockchainDevelopersTransaction] = transactionsMap.take(limit).values

  override type NVCT = BlockchainDevelopersMempool
}
