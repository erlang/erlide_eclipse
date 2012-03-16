package org.erlide.core.services.builder;

import java.util.Collection;

import org.eclipse.core.runtime.Assert;
import org.erlide.utils.TermParser;
import org.erlide.utils.TermParserException;
import org.erlide.utils.Tuple;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;

public abstract class CompilerOption {

    public static final BooleanOption COMPRESSED = new BooleanOption(
            "compressed",
            false,
            "Compress beam file",
            "The compiler will compress the generated object code, which can be useful for embedded systems");
    public static final BooleanOption DEBUG_INFO = new BooleanOption(
            "debug_info", false, "Debug info",
            "Include debug info in BEAM file");
    public static final BooleanOption ENCRYPT_DEBUG_INFO = new BooleanOption(
            "encrypt_debug_info", false, "Encrypt debug info",
            "Encrypt debug info, the key will be read from .erlang.crypt");
    public static final DefineOption DEFINE = new DefineOption("d",
            new String[] { "Name", "Value" }, "", "");

    public static final WarningOption WARN_UNUSED_RECORD = new WarningOption(
            "warn_unused_record", true, "Unused records", "Unused records");
    public static final WarningOption WARN_UNUSED_VARS = new WarningOption(
            "warn_unused_vars", true, "Unused variables", "Unused variables");
    public static final WarningOption WARN_UNUSED_IMPORT = new WarningOption(
            "warn_unused_import", false, "Unused imports",
            "Unused imported functions");
    public static final WarningOption WARN_OBSOLETE_GUARD = new WarningOption(
            "warn_obsolete_guard", false, "Obsolete guards",
            "Old type testing BIFs such as pid/1 and list/1");
    public static final WarningOption WARN_DEPRECATED_FUNCTION = new WarningOption(
            "warn_deprecated_function", true, "Deprecated functions",
            "Call to a function known by the compiler to be deprecated");
    public static final WarningOption WARN_UNUSED_FUNCTION = new WarningOption(
            "warn_unused_function", true, "Unused functions",
            "Local functions are not being called");
    public static final WarningOption WARN_SHADOW_VARS = new WarningOption(
            "warn_shadow_vars",
            false,
            "Shadowed variables",
            "Warn for \"fresh\" variables in functional objects or list comprehensions with the same name as some already defined variable");
    public static final WarningOption WARN_EXPORT_VARS = new WarningOption(
            "warn_export_vars",
            false,
            "Implicitely exported variables",
            "Warn for implicitly exported variables referred to after the primitives where they were first defined");
    public static final WarningOption WARN_EXPORT_ALL = new WarningOption(
            "warn_export_all", false, "Use of export_all",
            "The compiler option export_all");
    public static final WarningOption WARN_BIF_CLASH = new WarningOption(
            "warn_bif_clash",
            false,
            "Name clashes with BIFs",
            "an exported function with the same name as an auto-imported BIF (such as size/1)\n"
                    + " AND there is a call to it without a qualifying module name.");

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
            } else {
                return new OtpErlangAtom("no" + getName());
            }
        }
    }

    public static class DefineOption extends CompilerOption {

        private final String[] fieldLabels;

        public DefineOption(final String name, final String[] fieldLabels,
                final String description, final String tooltip) {
            super(name, description, tooltip);
            this.fieldLabels = fieldLabels;
            Assert.isLegal(fieldLabels.length == 2);
        }

        public String[] getFieldLabels() {
            return fieldLabels;
        }

        public OtpErlangList toTerm(
                final Collection<Tuple<String, String>> values)
                throws TermParserException {
            final OtpErlangObject[] defines = new OtpErlangObject[values.size()];
            final int i = 0;
            for (final Tuple<String, String> value : values) {
                final String key = value.first;
                final String val = value.second;

                final OtpErlangAtom tag = new OtpErlangAtom(getName());
                final OtpErlangAtom okey = new OtpErlangAtom(key);
                if (Strings.isNullOrEmpty(val)) {
                    defines[i] = OtpErlang.mkTuple(tag, okey);
                } else {
                    final OtpErlangObject ovalue = TermParser.getParser()
                            .parse(val);
                    defines[i] = OtpErlang.mkTuple(tag, okey, ovalue);
                }
            }
            return OtpErlang.mkList(defines);
        }

    }

    //@formatter:off    
    public static final Collection<WarningOption> WARNINGS = 
            Lists.newArrayList(
                    WARN_BIF_CLASH,
                    WARN_EXPORT_ALL,
                    WARN_EXPORT_VARS,
                    WARN_SHADOW_VARS,
                    WARN_UNUSED_FUNCTION,
                    WARN_DEPRECATED_FUNCTION,
                    WARN_OBSOLETE_GUARD,
                    WARN_UNUSED_IMPORT,
                    WARN_UNUSED_VARS,
                    WARN_UNUSED_RECORD);
    public static final Collection<CompilerOption> ALL_OPTIONS = 
            Lists.newArrayList(
                    COMPRESSED,
                    DEBUG_INFO,
                    ENCRYPT_DEBUG_INFO,
                    DEFINE,
                    WARN_BIF_CLASH,
                    WARN_EXPORT_ALL,
                    WARN_EXPORT_VARS,
                    WARN_SHADOW_VARS,
                    WARN_UNUSED_FUNCTION,
                    WARN_DEPRECATED_FUNCTION,
                    WARN_OBSOLETE_GUARD,
                    WARN_UNUSED_IMPORT,
                    WARN_UNUSED_VARS,
                    WARN_UNUSED_RECORD);
    //@formatter:on    

    private final String name;
    private final String description;
    private final String tooltip;

    private CompilerOption(final String name, final String description,
            final String tooltip) {
        this.name = name;
        this.description = description;
        this.tooltip = tooltip;
    }

    public static CompilerOption find(final String data) {
        for (final CompilerOption option : ALL_OPTIONS) {
            if (data.equals(option.getName())) {
                return option;
            }
        }
        throw new IllegalArgumentException(
                "Invalid value for compiler option: " + data);
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