package org.erlide.engine.model.root;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.engine.ErlangEngine;
import org.erlide.util.erlang.OtpBindings;
import org.erlide.util.erlang.OtpErlang;

import com.ericsson.otp.erlang.OtpErlangObject;

@SuppressWarnings("all")
public class EmakeConfigurationSerializer implements ProjectConfigurationSerializer {
    @Override
    public String encodeConfig(final ErlangProjectProperties info) {
        return null;
    }

    @Override
    public ErlangProjectProperties decodeConfig(final String config) {
        ErlangProjectProperties _xblockexpression = null;
        {
            final ErlangProjectProperties result = new ErlangProjectProperties();
            final Path _path = new Path("ebin");
            result.setOutputDir(_path);
            result.setSourceDirs();
            final List<OtpErlangObject> content = ErlangEngine.getInstance()
                    .getSimpleParserService().parse(config);
            final boolean _isEmpty = content.isEmpty();
            if (_isEmpty) {
                return result;
            }
            final Consumer<OtpErlangObject> _function = (
                    final OtpErlangObject erl_opts) -> {
                try {
                    final OtpBindings bindings = OtpErlang.match("{Src,Opts}", erl_opts);
                    if (bindings != null) {
                        final String src = bindings.getAtom("Src");
                        String _xifexpression = null;
                        final boolean _contains = src.contains("/");
                        if (_contains) {
                            _xifexpression = IterableExtensions
                                    .<String> head((Iterable<String>) Conversions
                                            .doWrapArray(src.split("/")));
                        } else {
                            _xifexpression = "src";
                        }
                        final String path = _xifexpression;
                        final Collection<IPath> _sourceDirs = result.getSourceDirs();
                        final ArrayList<IPath> sd = new ArrayList<>(_sourceDirs);
                        final Path _path_1 = new Path(path);
                        sd.add(_path_1);
                        result.setSourceDirs(sd);
                        final Collection<OtpErlangObject> opts = bindings.getList("Opts");
                        if (opts != null) {
                            final Consumer<OtpErlangObject> _function_1 = (
                                    final OtpErlangObject opt) -> {
                                try {
                                    final OtpBindings b = OtpErlang.match("{Tag,Arg}",
                                            opt);
                                    if (b != null) {
                                        parseOption(b, result);
                                    }
                                } catch (final Throwable _e) {
                                    throw Exceptions.sneakyThrow(_e);
                                }
                            };
                            opts.forEach(_function_1);
                        }
                    }
                } catch (final Throwable _e) {
                    throw Exceptions.sneakyThrow(_e);
                }
            };
            content.forEach(_function);
            _xblockexpression = result;
        }
        return _xblockexpression;
    }

    public void parseOption(final OtpBindings b, final ErlangProjectProperties result) {
        try {
            final String _atom = b.getAtom("Tag");
            if (_atom != null) {
                switch (_atom) {
                case "i":
                    final Collection<IPath> _includeDirs = result.getIncludeDirs();
                    final List<IPath> incs = new ArrayList<>(_includeDirs);
                    final String _string = b.getString("Arg");
                    final Path inc = new Path(_string);
                    final boolean _contains = incs.contains(inc);
                    final boolean _not = !_contains;
                    if (_not) {
                        incs.add(inc);
                    }
                    result.setIncludeDirs(incs);
                    break;
                }
            }
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }
}
