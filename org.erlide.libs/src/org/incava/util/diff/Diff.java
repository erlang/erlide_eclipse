package org.incava.util.diff;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;

/**
 * Compares two lists, returning a list of the additions, changes, and deletions
 * between them. A <code>Comparator</code> may be passed as an argument to the
 * constructor, and will thus be used. If not provided, the initial value in the
 * <code>a</code> ("from") list will be looked at to see if it supports the
 * <code>Comparable</code> interface. If so, its <code>equals</code> and
 * <code>compareTo</code> methods will be invoked on the instances in the "from"
 * and "to" lists; otherwise, for speed, hash codes from the objects will be
 * used instead for comparison.
 * 
 * <p>
 * The file FileDiff.java shows an example usage of this class, in an
 * application similar to the Unix "diff" program.
 * </p>
 */
public class Diff<Type> {
    /**
     * The source list, AKA the "from" values.
     */
    protected List<Type> a;

    /**
     * The target list, AKA the "to" values.
     */
    protected List<Type> b;

    /**
     * The list of differences, as <code>Difference</code> instances.
     */
    protected List<Difference> diffs = new ArrayList<Difference>();

    /**
     * The pending, uncommitted difference.
     */
    private Difference pending;

    /**
     * The comparator used, if any.
     */
    private Comparator<Type> comparator;

    /**
     * The thresholds.
     */
    private TreeMap<Integer, Integer> thresh;

    /**
     * Constructs the Diff object for the two arrays, using the given
     * comparator.
     */
    public Diff(final Type[] a, final Type[] b, final Comparator<Type> comp) {
        this(Arrays.asList(a), Arrays.asList(b), comp);
    }

    /**
     * Constructs the Diff object for the two arrays, using the default
     * comparison mechanism between the objects, such as <code>equals</code> and
     * <code>compareTo</code>.
     */
    public Diff(final Type[] a, final Type[] b) {
        this(a, b, null);
    }

    /**
     * Constructs the Diff object for the two lists, using the given comparator.
     */
    public Diff(final List<Type> a, final List<Type> b,
            final Comparator<Type> comp) {
        this.a = a;
        this.b = b;
        this.comparator = comp;
        this.thresh = null;
    }

    /**
     * Constructs the Diff object for the two lists, using the default
     * comparison mechanism between the objects, such as <code>equals</code> and
     * <code>compareTo</code>.
     */
    public Diff(final List<Type> a, final List<Type> b) {
        this(a, b, null);
    }

    /**
     * Runs diff and returns the results.
     */
    public List<Difference> diff() {
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

        final int lastA = a.size() - 1;
        final int lastB = b.size() - 1;
        int bi = 0;
        int ai;

        final int lastMatch = matches.length - 1;

        for (ai = 0; ai <= lastMatch; ++ai) {
            final Integer bLine = matches[ai];

            if (bLine == null) {
                onANotB(ai, bi);
            } else {
                while (bi < bLine) {
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
    protected boolean equals(final Type x, final Type y) {
        return comparator == null ? x.equals(y) : comparator.compare(x, y) == 0;
    }

    /**
     * Returns an array of the longest common subsequences.
     */
    public Integer[] getLongestCommonSubsequences() {
        int aStart = 0;
        int aEnd = a.size() - 1;

        int bStart = 0;
        int bEnd = b.size() - 1;

        final TreeMap<Integer, Integer> matches = new TreeMap<Integer, Integer>();

        while (aStart <= aEnd && bStart <= bEnd
                && equals(a.get(aStart), b.get(bStart))) {
            matches.put(aStart++, bStart++);
        }

        while (aStart <= aEnd && bStart <= bEnd
                && equals(a.get(aEnd), b.get(bEnd))) {
            matches.put(aEnd--, bEnd--);
        }

        Map<Type, List<Integer>> bMatches = null;
        if (comparator == null) {
            if (a.size() > 0 && a.get(0) instanceof Comparable) {
                // this uses the Comparable interface
                bMatches = new TreeMap<Type, List<Integer>>();
            } else {
                // this just uses hashCode()
                bMatches = new HashMap<Type, List<Integer>>();
            }
        } else {
            // we don't really want them sorted, but this is the only Map
            // implementation (as of JDK 1.4) that takes a comparator.
            bMatches = new TreeMap<Type, List<Integer>>(comparator);
        }

        for (int bi = bStart; bi <= bEnd; ++bi) {
            final Type element = b.get(bi);
            final Type key = element;
            List<Integer> positions = bMatches.get(key);

            if (positions == null) {
                positions = new ArrayList<Integer>();
                bMatches.put(key, positions);
            }

            positions.add(bi);
        }

        thresh = new TreeMap<Integer, Integer>();
        final Map<Integer, Object[]> links = new HashMap<Integer, Object[]>();

        for (int i = aStart; i <= aEnd; ++i) {
            final Type aElement = a.get(i);
            final List<Integer> positions = bMatches.get(aElement);

            if (positions != null) {
                Integer k = 0;
                final ListIterator<Integer> pit = positions
                        .listIterator(positions.size());
                while (pit.hasPrevious()) {
                    final Integer j = pit.previous();

                    k = insert(j, k);

                    if (k == null) {
                        // nothing
                    } else {
                        final Object value = k > 0 ? links.get(k - 1) : null;
                        links.put(k, new Object[] { value, i, j });
                    }
                }
            }
        }

        if (thresh.size() > 0) {
            final Integer ti = thresh.lastKey();
            Object[] link = links.get(ti);
            while (link != null) {
                final Integer x = (Integer) link[1];
                final Integer y = (Integer) link[2];
                matches.put(x, y);
                link = (Object[]) link[0];
            }
        }

        final int size = matches.size() == 0 ? 0 : 1 + matches.lastKey();
        final Integer[] ary = new Integer[size];
        for (final Integer idx : matches.keySet()) {
            final Integer val = matches.get(idx);
            ary[idx] = val;
        }
        return ary;
    }

    /**
     * Returns whether the integer is not zero (including if it is not null).
     */
    protected static boolean isNonzero(final Integer i) {
        return i != null && i != 0;
    }

    /**
     * Returns whether the value in the map for the given index is greater than
     * the given value.
     */
    protected boolean isGreaterThan(final Integer index, final Integer val) {
        final Integer lhs = thresh.get(index);
        return lhs != null && val != null && lhs.compareTo(val) > 0;
    }

    /**
     * Returns whether the value in the map for the given index is less than the
     * given value.
     */
    protected boolean isLessThan(final Integer index, final Integer val) {
        final Integer lhs = thresh.get(index);
        return lhs != null && (val == null || lhs.compareTo(val) < 0);
    }

    /**
     * Returns the value for the greatest key in the map.
     */
    protected Integer getLastValue() {
        return thresh.get(thresh.lastKey());
    }

    /**
     * Adds the given value to the "end" of the threshold map, that is, with the
     * greatest index/key.
     */
    protected void append(final Integer value) {
        Integer addIdx = null;
        if (thresh.size() == 0) {
            addIdx = 0;
        } else {
            final Integer lastKey = thresh.lastKey();
            addIdx = lastKey + 1;
        }
        thresh.put(addIdx, value);
    }

    /**
     * Inserts the given values into the threshold map.
     */
    protected Integer insert(final Integer j, Integer k) {
        if (isNonzero(k) && isGreaterThan(k, j) && isLessThan(k - 1, j)) {
            thresh.put(k, j);
        } else {
            int high = -1;

            if (isNonzero(k)) {
                high = k;
            } else if (thresh.size() > 0) {
                high = thresh.lastKey();
            }

            // off the end?
            if (high == -1 || j.compareTo(getLastValue()) > 0) {
                append(j);
                k = high + 1;
            } else {
                // binary search for insertion point:
                int low = 0;

                while (low <= high) {
                    final int index = (high + low) / 2;
                    final Integer val = thresh.get(index);
                    final int cmp = j.compareTo(val);

                    if (cmp == 0) {
                        return null;
                    } else if (cmp > 0) {
                        low = index + 1;
                    } else {
                        high = index - 1;
                    }
                }

                thresh.put(low, j);
                k = low;
            }
        }

        return k;
    }
}
