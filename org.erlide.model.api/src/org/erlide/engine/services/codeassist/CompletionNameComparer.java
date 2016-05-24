package org.erlide.engine.services.codeassist;

import java.util.Comparator;

public class CompletionNameComparer implements Comparator<CompletionData> {

    private final String prefix;

    public CompletionNameComparer(final String prefix) {
        this.prefix = prefix;
    }

    @Override
    public int compare(final CompletionData o1, final CompletionData o2) {
        final String s1 = o1.getDisplayString();
        final String s2 = o2.getDisplayString();
        // exact prefix matches get higher priority
        if (s1.startsWith(prefix) && s2.startsWith(prefix)) {
            return s1.compareTo(s2);
        }
        if (s1.startsWith(prefix)) {
            return -1;
        }
        if (s2.startsWith(prefix)) {
            return 1;
        }
        return s1.compareTo(s2);
    }

}
