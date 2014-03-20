package org.erlide.core.builder;

import java.util.ArrayList;
import java.util.Collection;
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
import org.erlide.util.erlang.TermParserException;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class CompilerOptions {

    private static final String QUALIFIER = ErlangCore.PLUGIN_ID + "/compiler";

    private final Map<CompilerOption, Object> options;
    private PreferencesHelper helper;

    public static OtpErlangList get(final IProject project) {
        final CompilerOptions prefs = new CompilerOptions(project);
        prefs.load();
        final OtpErlangList compilerOptions = prefs.export();
        return compilerOptions;
    }

    public CompilerOptions() {
        helper = PreferencesHelper.getHelper(QUALIFIER);
        options = Maps.newHashMap();
        for (final CompilerOption option : CompilerOption.ALL_OPTIONS) {
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
        helper = PreferencesHelper.getHelper(QUALIFIER, project);
    }

    public boolean hasOptionsAtLowestScope() {
        return helper.hasAnyAtLowestScope();
    }

    public void store() throws BackingStoreException {
        for (final CompilerOption option : CompilerOption.ALL_OPTIONS) {
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
            } else if (option instanceof ModuleOption) {
                if (value != null) {
                    final String avalue = (String) value;
                    helper.putString(option.getName(), avalue);
                } else {
                    helper.remove(option.getName());
                }
            } else if (option instanceof RawOption) {
                if (value != null) {
                    final String avalue = (String) value;
                    helper.putString(option.getName(), avalue);
                } else {
                    helper.remove(option.getName());
                }
            } else {
                if (value != null) {
                    @SuppressWarnings("unchecked")
                    final Collection<Pair<String, String>> val = (Collection<Pair<String, String>>) value;
                    helper.putString(option.getName(), val.toString());
                } else {
                    helper.remove(option.getName());
                }
            }
        }
        helper.flush();
    }

    public void load() {
        options.clear();
        for (final CompilerOption option : CompilerOption.ALL_OPTIONS) {
            final String value = helper.getString(option.getName(), null);
            if (option instanceof BooleanOption) {
                options.put(option, value != null ? Boolean.parseBoolean(value)
                        : ((BooleanOption) option).getDefaultValue());
            } else if (option instanceof PathsOption) {
                if (!Strings.isNullOrEmpty(value)) {
                    options.put(option, PathsOption.fromString(value));
                }
            } else if (option instanceof ModuleOption) {
                if (!Strings.isNullOrEmpty(value)) {
                    options.put(option, value);
                }
            } else if (option instanceof RawOption) {
                if (!Strings.isNullOrEmpty(value)) {
                    options.put(option, value);
                }
            } else {
                if (value != null) {
                    final String[] str = value.split(",");
                    options.put(option, new Pair<String, String>(str[0], str[1]));
                }
            }
        }
        options.put(CompilerOption.DEBUG_INFO, true);
    }

    @SuppressWarnings("unchecked")
    public OtpErlangList export() {
        final List<OtpErlangObject> result = new ArrayList<OtpErlangObject>();
        for (final CompilerOption option : CompilerOption.ALL_OPTIONS) {
            final Object optionValue = options.get(option);
            if (optionValue != null) {
                if (option instanceof BooleanOption) {
                    final OtpErlangObject val = ((BooleanOption) option)
                            .toTerm(((Boolean) optionValue).booleanValue());
                    if (val != null) {
                        result.add(val);
                    }
                } else if (option instanceof PathsOption) {
                    final Iterable<String> value = (Iterable<String>) optionValue;
                    final OtpErlangList val = (OtpErlangList) ((PathsOption) option)
                            .toTerm(value);
                    for (final OtpErlangObject inc : val.elements()) {
                        result.add(inc);
                    }
                } else if (option instanceof ModuleOption) {
                    final String value = (String) optionValue;
                    final OtpErlangObject val = ((ModuleOption) option).toTerm(value);
                    result.add(val);
                } else if (option instanceof RawOption) {
                    final String value = (String) optionValue;
                    final OtpErlangList val = (OtpErlangList) ((RawOption) option)
                            .toTerm(value);
                    for (final OtpErlangObject item : val.elements()) {
                        result.add(item);
                    }
                } else {
                    try {
                        final OtpErlangList val = ((DefineOption) option)
                                .toTerm((List<Pair<String, String>>) optionValue);
                        if (val != null) {
                            result.addAll(Lists.newArrayList(val.elements()));
                        }
                    } catch (final TermParserException e) {
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
            final Pair<String, String>... values) {
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
            return ((Boolean) value).booleanValue();
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
