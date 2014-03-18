package org.erlide.engine.services.correction;

import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.collect.Lists;

public class MessageMatcher {

    public Collection<String> matchMessage(final String message, final Pattern pattern) {
        final List<String> result = Lists.newArrayList();

        final Matcher matcher = pattern.matcher(message);
        if (!matcher.matches()) {
            return null;
        }
        final int num = matcher.groupCount();
        for (int i = 1; i <= num; i++) {
            result.add(matcher.group(i));
        }
        return result;
    }

}
