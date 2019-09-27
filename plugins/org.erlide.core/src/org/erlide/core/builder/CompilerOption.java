package org.erlide.core.builder;

import java.util.Collection;
import java.util.List;

import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.util.erlang.OtpErlang;
import org.erlide.util.erlang.OtpParser;
import org.erlide.util.erlang.OtpParserException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

public abstract class CompilerOption {

    public static class PathsOption extends CompilerOption {
        public PathsOption(final String name, final String description,
                final String tooltip) {
            super(name, description, tooltip);
        }

        public OtpErlangObject toTerm(final Iterable<String> value) {
            final List<OtpErlangObject> result = Lists.newArrayList();
            for (final String path : value) {
                result.add(OtpErlang.mkTuple(new OtpErlangAtom(getName()),
                        new OtpErlangString(path)));
            }
            return OtpErlang.mkList(result);
        }

        public static String toString(final Iterable<String> value) {
            return Joiner.on(',').join(value);
        }

        public static Iterable<String> fromString(final String string) {
            return Splitter.on(',').trimResults().omitEmptyStrings().split(string);
        }
    }

    public static class ModuleOption extends CompilerOption {
        public ModuleOption(final String name, final String description,
                final String tooltip) {
            super(name, description, tooltip);
        }

        public OtpErlangObject toTerm(final String value) {
            return OtpErlang.mkTuple(new OtpErlangAtom(getName()),
                    new OtpErlangAtom(value));
        }

        public static String toString(final String value) {
            return value;
        }

        public static String fromString(final String string) {
            return string;
        }
    }

    public static class RawOption extends CompilerOption {
        public RawOption(final String name, final String description,
                final String tooltip) {
            super(name, description, tooltip);
        }

        public OtpErlangObject toTerm(final String value) {
            final OtpParser parser = OtpErlang.getTermParser();
            OtpErlangObject result;
            try {
                String val;
                if (value.charAt(0) != '[') {
                    val = "[" + value + "]";
                } else {
                    val = value;
                }
                result = parser.parse(val);
            } catch (final Exception e) {
                result = OtpErlang.mkList();
            }
            return result;
        }

        public static String toString(final String value) {
            return value;
        }

        public static String fromString(final String string) {
            return string;
        }
    }

    public static class BooleanOption extends CompilerOption {
        private final boolean defaultValue;

        public BooleanOption(final String name, final boolean defaultValue,
                final String description, final String tooltip) {
            super(name, description, tooltip);
            this.defaultValue = defaultValue;
        }

        public boolean getDefaultValue() {
            return defaultValue;
        }

        public OtpErlangObject toTerm(final boolean currentValue) {
            if (currentValue) {
                return new OtpErlangAtom(getName());
            }
            return null;
        }

    }

    public static class WarningOption extends BooleanOption {

        public WarningOption(final String name, final boolean defaultValue,
                final String description, final String tooltip) {
            super(name, defaultValue, description, tooltip);
        }

        @Override
        public OtpErlangObject toTerm(final boolean currentValue) {
            if (currentValue) {
                return new OtpErlangAtom(getName());
            }
            return new OtpErlangAtom("no" + getName());
        }
    }

    public static class DefineOption extends CompilerOption {

        private final String[] fieldLabels;

        public DefineOption(final String name, final String[] fieldLabels,
                final String description, final String tooltip) {
            super(name, description, tooltip);
            this.fieldLabels = fieldLabels;
        }

        public String[] getFieldLabels() {
            return fieldLabels;
        }

        public OtpErlangList toTerm(final Collection<Pair<String, String>> values)
                throws OtpParserException {
            final OtpErlangObject[] defines = new OtpErlangObject[values.size()];
            final int i = 0;
            for (final Pair<String, String> value : values) {
                final String key = value.getKey();
                final String val = value.getValue();

                final OtpErlangAtom tag = new OtpErlangAtom(getName());
                final OtpErlangAtom okey = new OtpErlangAtom(key);
                if (Strings.isNullOrEmpty(val)) {
                    defines[i] = OtpErlang.mkTuple(tag, okey);
                } else {
                    final OtpErlangObject ovalue = OtpErlang.getTermParser().parse(val);
                    defines[i] = OtpErlang.mkTuple(tag, okey, ovalue);
                }
            }
            return OtpErlang.mkList(defines);
        }

    }

    private final String name;
    private final String description;
    private final String tooltip;

    CompilerOption(final String name, final String description, final String tooltip) {
        this.name = name;
        this.description = description;
        this.tooltip = tooltip;
    }

    public static CompilerOption find(final String data) {
        for (final CompilerOption option : CompilerOptions.ALL_OPTIONS) {
            if (data.equals(option.getName())) {
                return option;
            }
        }
        throw new IllegalArgumentException("Invalid value for compiler option: " + data);
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public String getTooltip() {
        return tooltip;
    }

}
