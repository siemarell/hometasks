package transaction

import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool

import scala.util.{Failure, Success, Try}

class BlockchainDevelopersMempool(txs: Map[ModifierId, BlockchainDevelopersTransaction]) extends MemoryPool[BlockchainDevelopersTransaction, BlockchainDevelopersMempool] {

  override def put(tx: BlockchainDevelopersTransaction): Try[BlockchainDevelopersMempool] = Try(
    if (this.contains(tx.id)){
      throw new Exception(s"Mempool already contains transanction with id ${tx.id}")
    }else {
      val newTxs = this.txs + (tx.id -> tx)
      new BlockchainDevelopersMempool(newTxs)
    }
  )

  override def put(txs: Iterable[BlockchainDevelopersTransaction]): Try[BlockchainDevelopersMempool] = Try(
    txs.foldLeft(this)((inter,tx)=> inter.put(tx) match {
      case Success(mempool) => mempool
      case Failure(e) => throw new Exception(e)
    })
  )

  override def putWithoutCheck(txs: Iterable[BlockchainDevelopersTransaction]): BlockchainDevelopersMempool = ???

  override def remove(tx: BlockchainDevelopersTransaction): BlockchainDevelopersMempool = new BlockchainDevelopersMempool(this.txs - tx.id)

  override def filter(condition: BlockchainDevelopersTransaction => Boolean): BlockchainDevelopersMempool = new BlockchainDevelopersMempool(
    this.txs.filter(kv => condition(kv._2))
  )

  override def getById(id: ModifierId): Option[BlockchainDevelopersTransaction] = this.txs.get(id)

  override def contains(id: ModifierId): Boolean = this.txs.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[BlockchainDevelopersTransaction] = this.txs.filter(kv=> ids.contains(kv._1)).values.toIndexedSeq

  override def size: Int = txs.size

  override def take(limit: Int): Iterable[BlockchainDevelopersTransaction] = txs.take(limit).values

  override type NVCT = BlockchainDevelopersMempool
}
