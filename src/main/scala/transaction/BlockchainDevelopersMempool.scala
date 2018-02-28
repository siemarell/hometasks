package transaction

import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool
import scala.util.Try

class BlockchainDevelopersMempool(transactionsMap: Map[ModifierId, BlockchainDevelopersTransaction] = Map.empty)
  extends MemoryPool[BlockchainDevelopersTransaction, BlockchainDevelopersMempool] {

  override def put(tx: BlockchainDevelopersTransaction): Try[BlockchainDevelopersMempool] = put(Seq(tx))

  override def put(txs: Iterable[BlockchainDevelopersTransaction]): Try[BlockchainDevelopersMempool] = Try{
    txs.foreach(tx =>require(tx.signatures.length == tx.inputs.length))
    putWithoutCheck(txs.take(BlockchainDevelopersMempool.Limit - transactionsMap.size))
  }

  override def putWithoutCheck(txs: Iterable[BlockchainDevelopersTransaction]): BlockchainDevelopersMempool = {
    val newTransactionsMap = transactionsMap ++ txs.foldLeft(Map.empty:Map[ModifierId, BlockchainDevelopersTransaction]){
      (acc, tx) => acc + (tx.id -> tx)
    }
    new BlockchainDevelopersMempool(newTransactionsMap)
  }

  override def remove(tx: BlockchainDevelopersTransaction): BlockchainDevelopersMempool =
    new BlockchainDevelopersMempool(transactionsMap - tx.id)

  override def filter(condition: BlockchainDevelopersTransaction => Boolean): BlockchainDevelopersMempool =
    new BlockchainDevelopersMempool(transactionsMap.filter(kv => condition(kv._2)))

  override def getById(id: ModifierId): Option[BlockchainDevelopersTransaction] = transactionsMap.get(id)

  override def contains(id: ModifierId): Boolean = transactionsMap.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[BlockchainDevelopersTransaction] =
    transactionsMap.filter(kv=> ids.contains(kv._1)).values.toIndexedSeq

  override def size: Int = transactionsMap.size

  override def take(limit: Int): Iterable[BlockchainDevelopersTransaction] = transactionsMap.take(limit).values

  override type NVCT = BlockchainDevelopersMempool
}

object BlockchainDevelopersMempool {
  val Limit = 100
}