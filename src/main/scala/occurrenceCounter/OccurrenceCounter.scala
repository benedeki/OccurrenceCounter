package occurrenceCounter

import scala.annotation.tailrec

/**
 * Generic class that serves the purpose of continuously counting the number of occurrences of item kinds. The class is
 * able to return the top and bottom items at any moment; top being the items with the highest occurrence count, bottom
 * with the lowest.
 * @tparam T  The type of the items to count
 */
class OccurrenceCounter[T] {

  private var items = Map.empty[T, OccurrenceCounter.Bucket[T]]
  private var topBucket: Option[OccurrenceCounter.Bucket[T]] = None
  private var bottomBucket: Option[OccurrenceCounter.Bucket[T]] = None
  private var _itemCount: Long = 0L

  def itemCount: Long = _itemCount

  def itemKindCount: Int = items.size

  /**
   * Count the item into the occurrence counter
   * @param item the item to count in
   */
  def addItem(item: T): OccurrenceCounter[T] = {
    _itemCount += 1
    val itemBucket =
      if (items.contains(item)) {
        val bucket = items(item)
        val newBucket = bucket.moveItemToHigherBucket(item)
        if (topBucket.forall(_.count < newBucket.count)) {
          topBucket = Option(newBucket)
        }
        if (bottomBucket.exists(_.items.isEmpty)) {
          // the original bottomBucket got empty, replace it with the new bucket
          bottomBucket = Option(newBucket)
        }
        newBucket
      } else {
        bottomBucket match {
          case Some(bb) if bb.count == 1 => bb.addItem(item)
          case Some(bb)                  =>
            val newBucket = new OccurrenceCounter.Bucket(1, item).squeezeBellow(bb)
            bottomBucket = Option(newBucket)
            newBucket
          case None                     => addFirstBucket(item)
        }
      }
    items = items + (item->itemBucket)
    this
  }

  /**
   * Counts all the items into the occurrence counter
   * @param items The sequence of items the count for occurrences
   */
  def addItems(items: Seq[T]):OccurrenceCounter[T] = {
    items.foreach(addItem)
    this
  }


  /**
   * Returns the items with most occurrences so far
   * @param count the number of items to return
   * @return      the sequence of items of length given by count parameter with the highest occurrence counts; if on the
   *              threshold there are multiple items with same occurrence count, any of the items can be returned to
   *              fulfill the count
   */
  def top(count: Int): Seq[T] = {
    @tailrec
    def getTop(c: Int, inBucket: Option[OccurrenceCounter.Bucket[T]], acc: Seq[T]): Seq[T] = {
      inBucket match {
        case None => acc
        case Some(bucket) if c <= bucket.items.size => bucket.items.take(c).toSeq ++ acc
        case Some(bucket) => getTop(c - bucket.items.size, bucket.lowerBucket, bucket.items.toSeq ++ acc)
      }
    }

    getTop(count, topBucket, Seq.empty).reverse
  }

  /**
   * Returns the items with least occurrences so far
   * @param count the number of items to return
   * @return      the sequence of items of length given by count parameter with the lowest occurrence counts; if on the
   *              threshold there are multiple items with same occurrence count, any of the items can be returned to
   *              fulfill the count
   */
  def bottom(count: Int): Seq[T] = {
    @tailrec
    def getBottom(c: Int, inBucket: Option[OccurrenceCounter.Bucket[T]], acc: Seq[T]): Seq[T] = {
      inBucket match {
        case None => acc
        case Some(bucket) if c <= bucket.items.size => bucket.items.take(c).toSeq ++ acc
        case Some(bucket) => getBottom(c - bucket.items.size, bucket.higherBucket, bucket.items.toSeq ++ acc)
      }
    }

    getBottom(count, bottomBucket, Seq.empty).reverse
  }

  /**
   * Same as top method but the returned sequence consist of tuple of item and its occurrence number
   * @param count the number of items to return
   * @return      the sequence of items of length given by count parameter with the highest occurrence counts, including
   *              the occurrence number; if on the threshold there are multiple items with same occurrence count, any of
   *              the items can be returned to fulfill the count
   */
  def topWithCounts(count: Int): Seq[(T, Int)] = {
    @tailrec
    def getTop(c: Int, inBucket: Option[OccurrenceCounter.Bucket[T]], acc: Seq[(T, Int)]): Seq[(T, Int)] = {
      inBucket match {
        case None => acc
        case Some(bucket) if c <= bucket.items.size => bucket.items.take(c).map((_, bucket.count)).toSeq ++ acc
        case Some(bucket) => getTop(c - bucket.items.size, bucket.lowerBucket, bucket.items.map((_, bucket.count)).toSeq ++ acc)
      }
    }

    getTop(count, topBucket, Seq.empty).reverse
  }

  /**
   * Same as bottom method but the returned sequence consist of tuple of item and its occurrence number
   * @param count the number of items to return
   * @return      the sequence of items of length given by count parameter with the lowest occurrence counts, including
   *              the occurrence number; if on the threshold there are multiple items with same occurrence count, any of
   *              the items can be returned to fulfill the count
   */
  def bottomWithoutCount(count: Int): Seq[(T, Int)] = {
    @tailrec
    def getBottom(c: Int, inBucket: Option[OccurrenceCounter.Bucket[T]], acc: Seq[(T, Int)]): Seq[(T, Int)] = {
      inBucket match {
        case None => acc
        case Some(bucket) if c <= bucket.items.size => bucket.items.take(c).map((_, bucket.count)).toSeq ++ acc
        case Some(bucket) => getBottom(c - bucket.items.size, bucket.higherBucket, bucket.items.map((_, bucket.count)).toSeq ++ acc)
      }
    }

    getBottom(count, bottomBucket, Seq.empty).reverse
  }

  private def addFirstBucket(item: T): OccurrenceCounter.Bucket[T] = {
    val newBucket = new OccurrenceCounter.Bucket[T](1, item)
    topBucket = Option(newBucket)
    bottomBucket = topBucket
    newBucket
  }
}

object OccurrenceCounter {
  private class Bucket[T](val count: Int) {

    def this(count: Int, newItem: T) {
      this(count)
      _items = Set(newItem)
    }

    private var _items = Set.empty[T]

    private var _higherBucket: Option[Bucket[T]] = None
    private var _lowerBucket: Option[Bucket[T]] = None

    def items: Set[T] = _items
    def higherBucket: Option[Bucket[T]] = _higherBucket
    def lowerBucket: Option[Bucket[T]] = _lowerBucket

    def unchain(): (Option[ Bucket[T]], Option[ Bucket[T]]) = {
      val hb = _higherBucket
      val lb = _lowerBucket
      _higherBucket = None
      _lowerBucket = None
      hb.foreach(_._lowerBucket = lb)
      lb.foreach(_._higherBucket = hb)
      (lb, hb)
    }

    def squeezeAbove(above: Bucket[T]): Bucket[T] = {
      unchain()
      _lowerBucket = Option(above)
      _higherBucket = above._higherBucket
      val thisOption = Option(this)
      above._higherBucket.foreach(_._lowerBucket = thisOption)
      above._higherBucket = thisOption
      this
    }

    def squeezeBellow(bellow: Bucket[T]): Bucket[T] = {
      unchain()
      _higherBucket = Option(bellow)
      _lowerBucket = bellow._lowerBucket
      val thisOption = Option(this)
      bellow._lowerBucket.foreach(_._higherBucket = thisOption)
      bellow._lowerBucket = thisOption
      this
    }


    def moveItemToHigherBucket(item: T): Bucket[T] = {
      val incCount = count + 1
      val result = _higherBucket match {
        case Some(hb) if hb.count == incCount => hb.addItem(item)
        case _                                => new Bucket(incCount, item).squeezeAbove(this)
      }
      _items = _items - item
      if (_items.isEmpty) {
        unchain()
      }
      result
    }

    def addItem(item: T): Bucket[T] = {
      _items = _items + item
      this
    }
  }
}