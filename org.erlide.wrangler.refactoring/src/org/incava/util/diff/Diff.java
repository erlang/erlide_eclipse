package org.incava.util.diff;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;

/**
 * Compares two collections, returning a list of the additions, changes, and
 * deletions between them. A <code>Comparator</code> may be passed as an
 * argument to the constructor, and will thus be used. If not provided, the
 * initial value in the <code>a</code> ("from") collection will be looked at to
 * see if it supports the <code>Comparable</code> interface. If so, its
 * <code>equals</code> and <code>compareTo</code> methods will be invoked on the
 * instances in the "from" and "to" collections; otherwise, for speed, hash
 * codes from the objects will be used instead for comparison.
 * 
 * <p>
 * The file FileDiff.java shows an example usage of this class, in an
 * application similar to the Unix "diff" program.
 * </p>
 */
public class Diff {
    /**
     * The source array, AKA the "from" values.
     */
    protected Object[] a;

    /**
     * The target array, AKA the "to" values.
     */
    protected Object[] b;

    /**
     * The list of differences, as <code>Difference</code> instances.
     */
    @SuppressWarnings({ "rawtypes" })
    protected List diffs = new ArrayList();

    /**
     * The pending, uncommitted difference.
     */
    private Difference pending;

    /**
     * The comparator used, if any.
     */
    @SuppressWarnings("rawtypes")
    private final Comparator comparator;

    /**
     * The thresholds.
     */
    @SuppressWarnings("rawtypes")
    private TreeMap thresh;

    /**
     * Constructs the Diff object for the two arrays, using the given
     * comparator.
     */
    @SuppressWarnings({})
    public Diff(final Object[] a, final Object[] b,
            @SuppressWarnings("rawtypes") final Comparator comp) {
        this.a = a;
        this.b = b;
        comparator = comp;
        thresh = null; // created in getLongestCommonSubsequences
    }

    /**
     * Constructs the Diff object for the two arrays, using the default
     * comparison mechanism between the objects, such as <code>equals</code> and
     * <code>compareTo</code>.
     */
    public Diff(final Object[] a, final Object[] b) {
        this(a, b, null);
    }

    /**
     * Constructs the Diff object for the two collections, using the given
     * comparator.
     */
    @SuppressWarnings({ "rawtypes" })
    public Diff(final Collection a, final Collection b, final Comparator comp) {
        this(a.toArray(), b.toArray(), comp);
    }

    /**
     * Constructs the Diff object for the two collections, using the default
     * comparison mechanism between the objects, such as <code>equals</code> and
     * <code>compareTo</code>.
     */
    @SuppressWarnings("rawtypes")
    public Diff(final Collection a, final Collection b) {
        this(a, b, null);
    }

    /**
     * Runs diff and returns the results.
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public List diff() {
        traverseSequences();

        // add the last difference, if pending:
        if (pending != null) {
            diffs.add(pending);
        }

        return diffs;
    }

    /**
     * Traverses the sequences, seeking the longest common subsequences,
     * invoking the methods <code>finishedA</code>, <code>finishedB</code>,
     * <code>onANotB</code>, and <code>onBNotA</code>.
     */
    protected void traverseSequences() {
        final Integer[] matches = getLongestCommonSubsequences();

        final int lastA = a.length - 1;
        final int lastB = b.length - 1;
        int bi = 0;
        int ai;

        final int lastMatch = matches.length - 1;

        for (ai = 0; ai <= lastMatch; ++ai) {
            final Integer bLine = matches[ai];

            if (bLine == null) {
                onANotB(ai, bi);
            } else {
                while (bi < bLine.intValue()) {
                    onBNotA(ai, bi++);
                }

                onMatch(ai, bi++);
            }
        }

        boolean calledFinishA = false;
        boolean calledFinishB = false;

        while (ai <= lastA || bi <= lastB) {

            // last A?
            if (ai == lastA + 1 && bi <= lastB) {
                if (!calledFinishA && callFinishedA()) {
                    finishedA(lastA);
                    calledFinishA = true;
                } else {
                    while (bi <= lastB) {
                        onBNotA(ai, bi++);
                    }
                }
            }

            // last B?
            if (bi == lastB + 1 && ai <= lastA) {
                if (!calledFinishB && callFinishedB()) {
                    finishedB(lastB);
                    calledFinishB = true;
                } else {
                    while (ai <= lastA) {
                        onANotB(ai++, bi);
                    }
                }
            }

            if (ai <= lastA) {
                onANotB(ai++, bi);
            }

            if (bi <= lastB) {
                onBNotA(ai, bi++);
            }
        }
    }

    /**
     * Override and return true in order to have <code>finishedA</code> invoked
     * at the last element in the <code>a</code> array.
     */
    protected boolean callFinishedA() {
        return false;
    }

    /**
     * Override and return true in order to have <code>finishedB</code> invoked
     * at the last element in the <code>b</code> array.
     */
    protected boolean callFinishedB() {
        return false;
    }

    /**
     * Invoked at the last element in <code>a</code>, if
     * <code>callFinishedA</code> returns true.
     */
    protected void finishedA(final int lastA) {
    }

    /**
     * Invoked at the last element in <code>b</code>, if
     * <code>callFinishedB</code> returns true.
     */
    protected void finishedB(final int lastB) {
    }

    /**
     * Invoked for elements in <code>a</code> and not in <code>b</code>.
     */
    protected void onANotB(final int ai, final int bi) {
        if (pending == null) {
            pending = new Difference(ai, ai, bi, -1);
        } else {
            pending.setDeleted(ai);
        }
    }

    /**
     * Invoked for elements in <code>b</code> and not in <code>a</code>.
     */
    protected void onBNotA(final int ai, final int bi) {
        if (pending == null) {
            pending = new Difference(ai, -1, bi, bi);
        } else {
            pending.setAdded(bi);
        }
    }

    /**
     * Invoked for elements matching in <code>a</code> and <code>b</code>.
     */
    @SuppressWarnings("unchecked")
    protected void onMatch(final int ai, final int bi) {
        if (pending == null) {
            // no current pending
        } else {
            diffs.add(pending);
            pending = null;
        }
    }

    /**
     * Compares the two objects, using the comparator provided with the
     * constructor, if any.
     */
    @SuppressWarnings("unchecked")
    protected boolean equals(final Object x, final Object y) {
        return comparator == null ? x.equals(y) : comparator.compare(x, y) == 0;
    }

    /**
     * Returns an array of the longest common subsequences.
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public Integer[] getLongestCommonSubsequences() {
        int aStart = 0;
        int aEnd = a.length - 1;

        int bStart = 0;
        int bEnd = b.length - 1;

        final TreeMap matches = new TreeMap();

        while (aStart <= aEnd && bStart <= bEnd && equals(a[aStart], b[bStart])) {
            matches.put(new Integer(aStart++), new Integer(bStart++));
        }

        while (aStart <= aEnd && bStart <= bEnd && equals(a[aEnd], b[bEnd])) {
            matches.put(new Integer(aEnd--), new Integer(bEnd--));
        }

        Map bMatches = null;
        if (comparator == null) {
            if (a.length > 0 && a[0] instanceof Comparable) {
                // this uses the Comparable interface
                bMatches = new TreeMap();
            } else {
                // this just uses hashCode()
                bMatches = new HashMap();
            }
        } else {
            // we don't really want them sorted, but this is the only Map
            // implementation (as of JDK 1.4) that takes a comparator.
            bMatches = new TreeMap(comparator);
        }

        for (int bi = bStart; bi <= bEnd; ++bi) {
            final Object element = b[bi];
            final Object key = element;
            List positions = (List) bMatches.get(key);
            if (positions == null) {
                positions = new ArrayList();
                bMatches.put(key, positions);
            }
            positions.add(new Integer(bi));
        }

        thresh = new TreeMap();
        final Map links = new HashMap();

        for (int i = aStart; i <= aEnd; ++i) {
            final Object aElement = a[i]; // keygen here.
            final List positions = (List) bMatches.get(aElement);

            if (positions != null) {
                Integer k = new Integer(0);
                final ListIterator pit = positions.listIterator(positions
                        .size());
                while (pit.hasPrevious()) {
                    final Integer j = (Integer) pit.previous();

                    k = insert(j, k);

                    if (k == null) {
                        // nothing
                    } else {
                        final Object value = k.intValue() > 0 ? links
                                .get(new Integer(k.intValue() - 1)) : null;
                        links.put(k, new Object[] { value, new Integer(i), j });
                    }
                }
            }
        }

        if (thresh.size() > 0) {
            final Integer ti = (Integer) thresh.lastKey();
            Object[] link = (Object[]) links.get(ti);
            while (link != null) {
                final Integer x = (Integer) link[1];
                final Integer y = (Integer) link[2];
                matches.put(x, y);
                link = (Object[]) link[0];
            }
        }

        return toArray(matches);
    }

    /**
     * Converts the map (indexed by java.lang.Integers) into an array.
     */
    protected static Integer[] toArray(
            @SuppressWarnings("rawtypes") final TreeMap map) {
        final int size = map.size() == 0 ? 0 : 1 + ((Integer) map.lastKey())
                .intValue();
        final Integer[] ary = new Integer[size];
        @SuppressWarnings("rawtypes")
        final Iterator it = map.keySet().iterator();

        while (it.hasNext()) {
            final Integer idx = (Integer) it.next();
            final Integer val = (Integer) map.get(idx);
            ary[idx.intValue()] = val;
        }
        return ary;
    }

    /**
     * Returns whether the integer is not zero (including if it is not null).
     */
    protected static boolean isNonzero(final Integer i) {
        return i != null && i.intValue() != 0;
    }

    /**
     * Returns whether the value in the map for the given index is greater than
     * the given value.
     */
    protected boolean isGreaterThan(final Integer index, final Integer val) {
        final Integer lhs = (Integer) thresh.get(index);
        return lhs != null && val != null && lhs.compareTo(val) > 0;
    }

    /**
     * Returns whether the value in the map for the given index is less than the
     * given value.
     */
    protected boolean isLessThan(final Integer index, final Integer val) {
        final Integer lhs = (Integer) thresh.get(index);
        return lhs != null && (val == null || lhs.compareTo(val) < 0);
    }

    /**
     * Returns the value for the greatest key in the map.
     */
    protected Integer getLastValue() {
        return (Integer) thresh.get(thresh.lastKey());
    }

    /**
     * Adds the given value to the "end" of the threshold map, that is, with the
     * greatest index/key.
     */
    @SuppressWarnings("unchecked")
    protected void append(final Integer value) {
        Integer addIdx = null;
        if (thresh.size() == 0) {
            addIdx = new Integer(0);
        } else {
            final Integer lastKey = (Integer) thresh.lastKey();
            addIdx = new Integer(lastKey.intValue() + 1);
        }
        thresh.put(addIdx, value);
    }

    /**
     * Inserts the given values into the threshold map.
     */
    @SuppressWarnings("unchecked")
    protected Integer insert(final Integer j, Integer k) {
        if (isNonzero(k) && isGreaterThan(k, j)
                && isLessThan(new Integer(k.intValue() - 1), j)) {
            thresh.put(k, j);
        } else {
            int hi = -1;

            if (isNonzero(k)) {
                hi = k.intValue();
            } else if (thresh.size() > 0) {
                hi = ((Integer) thresh.lastKey()).intValue();
            }

            // off the end?
            if (hi == -1 || j.compareTo(getLastValue()) > 0) {
                append(j);
                k = new Integer(hi + 1);
            } else {
                // binary search for insertion point:
                int lo = 0;

                while (lo <= hi) {
                    final int index = (hi + lo) / 2;
                    final Integer val = (Integer) thresh
                            .get(new Integer(index));
                    final int cmp = j.compareTo(val);

                    if (cmp == 0) {
                        return null;
                    } else if (cmp > 0) {
                        lo = index + 1;
                    } else {
                        hi = index - 1;
                    }
                }

                thresh.put(new Integer(lo), j);
                k = new Integer(lo);
            }
        }

        return k;
    }

}
