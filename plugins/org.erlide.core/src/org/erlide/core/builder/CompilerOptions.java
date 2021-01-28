package org.erlide.core.builder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.core.ErlangCore;
import org.erlide.core.builder.CompilerOption.BooleanOption;
import org.erlide.core.builder.CompilerOption.DefineOption;
import org.erlide.core.builder.CompilerOption.ModuleOption;
import org.erlide.core.builder.CompilerOption.PathsOption;
import org.erlide.core.builder.CompilerOption.RawOption;
import org.erlide.core.builder.CompilerOption.WarningOption;
import org.erlide.engine.util.PreferencesHelper;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.OtpErlang;
import org.erlide.util.erlang.OtpParserException;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class CompilerOptions {

    private static final String QUALIFIER = ErlangCore.PLUGIN_ID + "/compiler";

    public static final BooleanOption COMPRESSED = new BooleanOption("compressed", false,
            "Compress beam file",
            "The compiler will compress the generated object code, which can be useful for embedded systems");
    public static final BooleanOption DEBUG_INFO = new BooleanOption("debug_info", false,
            "Debug info", "Include debug info in BEAM file");
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
            "warn_unused_import", false, "Unused imports", "Unused imported functions");
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
            "warn_shadow_vars", false, "Shadowed variables",
            "Warn for \"fresh\" variables in functional objects or list comprehensions with the same name as some already defined variable");
    public static final WarningOption WARN_EXPORT_VARS = new WarningOption(
            "warn_export_vars", false, "Implicitely exported variables",
            "Warn for implicitly exported variables referred to after the primitives where they were first defined");
    public static final WarningOption WARN_EXPORT_ALL = new WarningOption(
            "warn_export_all", false, "Use of export_all",
            "The compiler option export_all");
    public static final PathsOption INCLUDE_DIRS = new PathsOption("i",
            "Additional include dirs: ", "Comma-separated list of paths");
    public static final ModuleOption PARSE_TRANSFORM = new ModuleOption("parse_transform",
            "Global parse transform module: ",
            "Specify a module to be used as a global parse transform");

    public static final RawOption CUSTOM = new RawOption("raw", "Other options: ",
            "A list of compiler options as Erlang terms (the list brackets can be omitted).");

    //@formatter:off
    public static final Collection<WarningOption> WARNINGS =
            Lists.newArrayList(
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
                    CUSTOM,
                    INCLUDE_DIRS,
                    PARSE_TRANSFORM,
                    DEFINE,
                    COMPRESSED,
                    DEBUG_INFO,
                    ENCRYPT_DEBUG_INFO,
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

    private final Map<CompilerOption, Object> options;
    private PreferencesHelper helper;

    public static OtpErlangList get(final IProject project) {
        final CompilerOptions prefs = new CompilerOptions(project);
        prefs.load();
        final OtpErlangList compilerOptions = prefs.export();
        return compilerOptions;
    }

    public CompilerOptions() {
        helper = PreferencesHelper.getHelper(CompilerOptions.QUALIFIER);
        options = Maps.newHashMap();
        for (final CompilerOption option : ALL_OPTIONS) {
            if (option instanceof BooleanOption) {
                options.put(option, ((BooleanOption) option).getDefaultValue());
            }
            if (option instanceof DefineOption) {
                options.put(option, new ArrayList<Pair<String, String>>());
            }
        }
    }

    public CompilerOptions(final IProject project) {
        this();
        helper = PreferencesHelper.getHelper(CompilerOptions.QUALIFIER, project);
    }

    public boolean hasOptionsAtLowestScope() {
        return helper.hasAnyAtLowestScope();
    }

    public void store() throws BackingStoreException {
        for (final CompilerOption option : ALL_OPTIONS) {
            final Object value = options.get(option);
            if (option instanceof BooleanOption) {
                final Boolean val = (Boolean) value;
                helper.putString(option.getName(), val.toString());
            } else if (option instanceof PathsOption) {
                if (value != null) {
                    @SuppressWarnings("unchecked")
                    final Iterable<String> avalue = (Iterable<String>) value;
                    helper.putString(option.getName(), PathsOption.toString(avalue));
                } else {
                    helper.remove(option.getName());
                }
            } else if (option instanceof ModuleOption || option instanceof RawOption) {
                if (value != null) {
                    final String avalue = (String) value;
                    helper.putString(option.getName(), avalue);
                } else {
                    helper.remove(option.getName());
                }
            } else if (value != null) {
                @SuppressWarnings("unchecked")
                final Collection<Pair<String, String>> val = (Collection<Pair<String, String>>) value;
                helper.putString(option.getName(), val.toString());
            } else {
                helper.remove(option.getName());
            }
        }
        helper.flush();
    }

    public void load() {
        options.clear();
        for (final CompilerOption option : ALL_OPTIONS) {
            final String value = helper.getString(option.getName(), null);
            if (option instanceof BooleanOption) {
                options.put(option, value != null ? Boolean.parseBoolean(value)
                        : ((BooleanOption) option).getDefaultValue());
            } else if (option instanceof PathsOption) {
                if (!Strings.isNullOrEmpty(value)) {
                    options.put(option, PathsOption.fromString(value));
                }
            } else if (option instanceof ModuleOption || option instanceof RawOption) {
                if (!Strings.isNullOrEmpty(value)) {
                    options.put(option, value);
                }
            } else if (value != null) {
                final String[] str = value.split(",");
                options.put(option, new Pair<>(str[0], str[1]));
            }
        }
        options.put(CompilerOptions.DEBUG_INFO, true);
    }

    @SuppressWarnings("unchecked")
    public OtpErlangList export() {
        final List<OtpErlangObject> result = new ArrayList<>();
        for (final CompilerOption option : ALL_OPTIONS) {
            final Object optionValue = options.get(option);
            if (optionValue != null) {
                if (option instanceof BooleanOption) {
                    final OtpErlangObject val = ((BooleanOption) option)
                            .toTerm((Boolean) optionValue);
                    if (val != null) {
                        result.add(val);
                    }
                } else if (option instanceof PathsOption) {
                    final Iterable<String> value = (Iterable<String>) optionValue;
                    final OtpErlangList val = (OtpErlangList) ((PathsOption) option)
                            .toTerm(value);
                    Collections.addAll(result, val.elements());
                } else if (option instanceof ModuleOption) {
                    final String value = (String) optionValue;
                    final OtpErlangObject val = ((ModuleOption) option).toTerm(value);
                    result.add(val);
                } else if (option instanceof RawOption) {
                    final String value = (String) optionValue;
                    final OtpErlangList val = (OtpErlangList) ((RawOption) option)
                            .toTerm(value);
                    Collections.addAll(result, val.elements());
                } else {
                    try {
                        final OtpErlangList val = ((DefineOption) option)
                                .toTerm((List<Pair<String, String>>) optionValue);
                        if (val != null) {
                            result.addAll(Lists.newArrayList(val.elements()));
                        }
                    } catch (final OtpParserException e) {
                        ErlLogger.warn(e);
                    }
                }
            }
        }
        final OtpErlangList list = OtpErlang.mkList(result);
        return list;
    }

    @Override
    public String toString() {
        return export().toString();
    }

    public void removeAllProjectSpecificSettings() {
        helper.removeAllAtLowestScope();
    }

    public void removeOption(final CompilerOption opt) {
        if (opt instanceof WarningOption) {
            options.put(opt, ((WarningOption) opt).getDefaultValue());
        } else {
            options.remove(opt);
        }
    }

    public void setPathOption(final CompilerOption opt, final Iterable<String> value) {
        if (value == null || !value.iterator().hasNext()) {
            removeOption(opt);
        } else {
            options.put(opt, value);
        }
    }

    public void setSimpleOption(final CompilerOption opt, final String value0) {
        final String value = value0.trim();
        if (Strings.isNullOrEmpty(value)) {
            removeOption(opt);
        } else {
            options.put(opt, value);
        }
    }

    public void setListOption(final CompilerOption opt,
            final List<Pair<String, String>> value) {
        if (value == null || value.isEmpty()) {
            removeOption(opt);
        } else {
            options.put(opt, value);
        }
    }

    public void setListOption(final CompilerOption opt,
            @SuppressWarnings("unchecked") final Pair<String, String>... values) {
        if (values == null || values.length == 0) {
            removeOption(opt);
        } else {
            final List<Pair<String, String>> list = Lists.newArrayList(values);
            setListOption(opt, list);
        }
    }

    public void setBooleanOption(final CompilerOption opt, final boolean selection) {
        options.put(opt, selection);
    }

    public boolean getBooleanOption(final CompilerOption opt) {
        final Object value = options.get(opt);
        if (opt instanceof BooleanOption) {
            return (Boolean) value;
        }
        return value != null;
    }

    @SuppressWarnings("unchecked")
    public Iterable<String> getPathsOption(final PathsOption option) {
        return (Iterable<String>) options.get(option);
    }

    public String getSimpleOption(final ModuleOption option) {
        return (String) options.get(option);
    }

    public String getSimpleOption(final RawOption option) {
        return (String) options.get(option);
    }

}
