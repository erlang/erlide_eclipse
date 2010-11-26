package org.erlide.core.util;

import java.util.ArrayList;
import java.util.List;

public class ArrayUtils {

    /**
     * @param listToFilter
     *            the list that should be filtered.
     * @param callbackThatFilters
     *            if true is returned in the callback, that means that the
     *            object should be added to the resulting list.
     */
    public static <T> List<T> filter(final T[] listToFilter,
            final ICallback<Boolean, T> callbackThatFilters) {
        final ArrayList<T> lst = new ArrayList<T>();
        for (final T marker : listToFilter) {
            if (callbackThatFilters.call(marker)) {
                lst.add(marker);
            }
        }
        return lst;
    }

}
