/**
 * Copyright (C) 2001-@year@ by University of Maryland, College Park,
 * MD 20742, USA All rights reserved.
 */
package com.appliedminds.martini.animation;

import java.util.ArrayList;

/**
 * <b>PriorityQueue</b> is a heap based PriorityQueue that stores
 * objects of type java.lang.Comparable. The default ordering is by
 * maximum value.
 *
 * @author Jesse Grosjean
 */
public class PriorityQueue {

	public static final int QUEUE_MAXIMUM_ORDERING = 0;
	public static final int QUEUE_MINIMUM_ORDERING = 1;

	private ArrayList fElements;
	private int fQueueOrdering;

	/**
	 * Construct a new priority queue, ordering by maximum value.
	 */
	public PriorityQueue() {
		this(QUEUE_MAXIMUM_ORDERING);
	}

	/**
	 * Construct a new priority queue.
	 *
	 * @param aOrdering determines the ordering of items stored in the
	 * queue.  use PriorityQueue.QUEUE_MAXIMUM_ORDERING or
	 * PriorityQueue.QUEUE_MINIMUM_ORDERING
	 */
	public PriorityQueue(int aOrdering) {
		super();
		fQueueOrdering = aOrdering;
	}

	/**
	 * Insert a new item into the queue.
	 */
	public void insert(Comparable aComparable) {
		expand();
		int i = size() - 1;
		while (i > 0 && !isProposedOrderCorrect(get(parent(i)), aComparable)) {
			set(i, get(parent(i)));
			i = parent(i);
		}
		set(i, aComparable);
	}

	/**
	 * Return the first object in the queue. This will either be the
	 * maximum or minimum valued object as determined by the queue
	 * ordering. This operation does not remove the object from the
	 * queue.
	 */
	public Object first() {
		return get(0);
	}

	/**
	 * Return the first object in the queue. This will either be the
	 * maximum or minimum valued object as determined by the queue
	 * ordering. This operation removes the object from the queue.
	 */
	public Object extractFirst() {
		Object first = get(0);
		set(0, get(size() - 1));
		contract();
		heapify(0);
		return first;
	}

	/**
	 * Return the number of items currently in the queue.
	 */
	public int size() {
		return elements().size();
	}

	/**
	 * Return true if the queue is empty.
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	protected ArrayList elements() {
		if (fElements == null) {
			fElements = new ArrayList();
		}
		return fElements;
	}

	protected int parent(int index) {
		return index / 2;
	}

	protected int left(int index) {
		return 2 * index;
	}

	protected int right(int index) {
		return (2 * index) + 1;
	}

	protected void heapify(int index) {
		int left = left(index);
		int right = right(index);
		int first = -1;

		if (left < size() && isProposedOrderCorrect(get(left), get(index))) {
			first = left;
		} else {
			first = index;
		}

		if (right < size() && isProposedOrderCorrect(get(right), get(first))) {
			first = right;
		}

		if (first != index) {
			exchange(index, first);
			heapify(first);
		}
	}

	protected void exchange(int a, int b) {
		Comparable temp = get(a);
		set(a, get(b));
		set(b, temp);
	}

	protected Comparable get(int index) {
		return (Comparable) elements().get(index);
	}

	protected void set(int index, Comparable aComparable) {
		elements().set(index, aComparable);
	}

	protected void expand() {
		elements().add(null);
	}

	protected void contract() {
		elements().remove(size() - 1);
	}

	protected boolean isProposedOrderCorrect(Comparable aProposedFirst, 
																					 Comparable aProposedSecond) 
	{
		int compareValue = aProposedFirst.compareTo(aProposedSecond);

		switch (fQueueOrdering) {
		case QUEUE_MAXIMUM_ORDERING:
			return !(compareValue < 0); // ordering is ok, unless the first
			// is less then the second.
		case QUEUE_MINIMUM_ORDERING:
			return !(compareValue > 0); // ordering is ok, unless the first
			// is greater then the second.
		}

		throw new RuntimeException("Bad Ordering State in PriorityQueue");
	}
}
