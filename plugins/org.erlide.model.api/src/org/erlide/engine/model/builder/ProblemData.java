package org.erlide.engine.model.builder;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;

@SuppressWarnings("all")
public class ProblemData extends ProblemData0 {
    public static final String TAG = "erlide.tag";

    public static final String ARGS = "erlide.args";

    private Pattern _pattern;

    public ProblemData(final String tag, final String message, final int arity) {
        super(tag, message, arity);
    }

    public Pattern getPattern() {
        if (_pattern == null) {
            final String str = ProblemData.quoteRegex(getMessage());
            final String key = "@@@";
            _pattern = Pattern.compile(str.replaceAll("\\\\~", key)
                    .replaceAll("~", "(.+?)").replaceAll(key, "~"));
        }
        return _pattern;
    }

    public void setPattern(final Pattern p) {
        throw new UnsupportedOperationException("pattern is read-only");
    }

    public String getCategory() {
        return IterableExtensions.<String> head(
                (Iterable<String>) Conversions.doWrapArray(getTag().split("_")));
    }

    public List<String> getMessageArgs(final String msg) {
        final Matcher matcher = getPattern().matcher(msg);
        final boolean _matches = matcher.matches();
        if (_matches) {
            final int num = matcher.groupCount();
            final ArrayList<String> result = CollectionLiterals.<String> newArrayList();
            int i = 1;
            while (i <= num) {
                {
                    result.add(matcher.group(i));
                    i = i + 1;
                }
            }
            return result;
        }
        return null;
    }

    public static String quoteRegex(final String string) {
        final byte[] esc = "([{^$|)?*+.".getBytes();
        String result = string;
        for (final byte c : esc) {
            {
                final String r = "\\" + Character.valueOf((char) c);
                final String v = "\\\\" + Character.valueOf((char) c);
                result = result.replaceAll(r, v);
            }
        }
        return result;
    }
}
