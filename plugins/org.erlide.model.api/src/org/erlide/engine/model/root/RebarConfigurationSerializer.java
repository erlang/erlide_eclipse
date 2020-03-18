package org.erlide.engine.model.root;

import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.erlide.engine.ErlangEngine;
import org.erlide.util.erlang.OtpBindings;
import org.erlide.util.erlang.OtpErlang;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

@SuppressWarnings("all")
public class RebarConfigurationSerializer implements ProjectConfigurationSerializer {
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
            final List<OtpErlangObject> content = ErlangEngine.getInstance()
                    .getSimpleParserService().parse(config);
            final boolean _isEmpty = content.isEmpty();
            if (_isEmpty) {
                return result;
            }
            final Consumer<OtpErlangObject> _function = (
                    final OtpErlangObject erl_opts) -> {
                try {
                    final OtpBindings bindings = OtpErlang.match("{erl_opts,Opts}",
                            erl_opts);
                    if (bindings != null) {
                        final Collection<OtpErlangObject> opts = bindings.getList("Opts");
                        if (opts != null) {
                            final Consumer<OtpErlangObject> _function_1 = (
                                    final OtpErlangObject opt) -> {
                                try {
                                    final OtpBindings b = OtpErlang.match("{Tag,Arg}",
                                            opt);
                                    if (b != null) {
                                        parseOption(result, b);
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

    public void parseOption(final ErlangProjectProperties result, final OtpBindings b) {
        try {
            final String _atom = b.getAtom("Tag");
            if (_atom != null) {
                switch (_atom) {
                case "i":
                    final String _string = b.getString("Arg");
                    final Path inc = new Path(_string);
                    final boolean _contains = result.getIncludeDirs().contains(inc);
                    final boolean _not = !_contains;
                    if (_not) {
                        final List<IPath> incs = CollectionLiterals.<IPath> newArrayList(
                                (IPath[]) Conversions.unwrapArray(result.getIncludeDirs(),
                                        IPath.class));
                        incs.add(inc);
                        result.setIncludeDirs(incs);
                    }
                    break;
                case "src_dirs":
                    final Function1<OtpErlangObject, Path> _function = (
                            final OtpErlangObject it) -> {
                        Path _xblockexpression = null;
                        {
                            String _xifexpression = null;
                            if (it instanceof OtpErlangString) {
                                _xifexpression = ((OtpErlangString) it).stringValue();
                            } else {
                                _xifexpression = it.toString();
                            }
                            final String s = _xifexpression;
                            _xblockexpression = new Path(s);
                        }
                        return _xblockexpression;
                    };
                    result.setSourceDirs((IPath[]) Conversions
                            .unwrapArray(IterableExtensions.<OtpErlangObject, Path> map(
                                    b.getList("Arg"), _function), IPath.class));
                    break;
                }
            }
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }
}
