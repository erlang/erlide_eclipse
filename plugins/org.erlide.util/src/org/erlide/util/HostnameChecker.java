package org.erlide.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.osgi.framework.Bundle;

import com.google.common.base.Objects;

@SuppressWarnings("all")
public class HostnameChecker {
    private static class Holder {
        private static final HostnameChecker INSTANCE = new HostnameChecker();
    }

    private static final String longNameFallback = "127.0.0.1";

    private static final String shortNameFallback = "localhost";

    private String longName;

    private String shortName;

    private HostnameChecker() {
    }

    public static HostnameChecker getInstance() {
        return HostnameChecker.Holder.INSTANCE;
    }

    public String getErlangHostName(final boolean useLongName) {
        String _xifexpression = null;
        if (useLongName) {
            _xifexpression = longName;
        } else {
            _xifexpression = shortName;
        }
        return _xifexpression;
    }

    public boolean isThisHost(final String host) {
        return Objects.equal(host, longName) || Objects.equal(host, shortName);
    }

    public boolean canUseLongNames() {
        return longName != null;
    }

    public boolean canUseShortNames() {
        return shortName != null;
    }

    public String getJavaLongHostName() {
        String _xblockexpression = null;
        {
            String name = null;
            try {
                final InetAddress addr = InetAddress.getLocalHost();
                name = addr.getCanonicalHostName();
                final boolean _contains = name.contains(".");
                final boolean _not = !_contains;
                if (_not) {
                    name = null;
                }
            } catch (final Throwable _t) {
                if (_t instanceof UnknownHostException) {
                } else {
                    throw Exceptions.sneakyThrow(_t);
                }
            }
            _xblockexpression = name;
        }
        return _xblockexpression;
    }

    public String getJavaShortHostName() {
        String _xblockexpression = null;
        {
            String name = null;
            try {
                final InetAddress addr = InetAddress.getLocalHost();
                name = addr.getHostName();
                final boolean _contains = name.contains(".");
                if (_contains) {
                    name = null;
                }
            } catch (final Throwable _t) {
                if (_t instanceof UnknownHostException) {
                } else {
                    throw Exceptions.sneakyThrow(_t);
                }
            }
            _xblockexpression = name;
        }
        return _xblockexpression;
    }

    /**
     * Start erlang nodes and find out how they resolve the long/short host names.
     */
    public boolean detectHostNames(final String otpHome) {
        notifyDeprecatedUsage();
        final ErlangHostnameRetriever retriever = new ErlangHostnameRetriever(otpHome);
        final boolean loadProperties = getErlideHostsFromProperties();
        if (loadProperties) {
            return true;
        }
        final boolean loaded = loadErlideHosts(getHostsFileName());
        if (loaded) {
            if (retriever.canConnect(shortName, false)
                    || retriever.canConnect(longName, true)) {
                return true;
            }
        }
        final Function0<String> _function = () -> longName;
        final Function0<String> _function_1 = () -> retriever.getErlangHostName(true);
        final Function0<String> _function_2 = () -> getJavaLongHostName();
        final Function0<String> _function_3 = () -> HostnameChecker.longNameFallback;
        final Iterable<Function0<? extends String>> longValues = Collections
                .<Function0<? extends String>> unmodifiableList(
                        CollectionLiterals.<Function0<? extends String>> newArrayList(
                                _function, _function_1, _function_2, _function_3));
        final Function1<String, Boolean> _function_4 = (final String it) -> Boolean
                .valueOf(it != null && retriever.canConnect(it, true));
        longName = findFirstValue(longValues, _function_4);
        final Function0<String> _function_5 = () -> shortName;
        final Function0<String> _function_6 = () -> retriever.getErlangHostName(false);
        final Function0<String> _function_7 = () -> getJavaShortHostName();
        final Function0<String> _function_8 = () -> HostnameChecker.shortNameFallback;
        final Iterable<Function0<? extends String>> shortValues = Collections
                .<Function0<? extends String>> unmodifiableList(
                        CollectionLiterals.<Function0<? extends String>> newArrayList(
                                _function_5, _function_6, _function_7, _function_8));
        final Function1<String, Boolean> _function_9 = (final String it) -> Boolean
                .valueOf(it != null && retriever.canConnect(it, false));
        shortName = findFirstValue(shortValues, _function_9);
        ErlLogger.debug("Detected:: \'%s\' && \'%s\'", shortName, longName);
        return canUseLongNames() || canUseShortNames();
    }

    public boolean getErlideHostsFromProperties() {
        shortName = System.getProperty("erlide.host.short");
        longName = System.getProperty("erlide.host.long");
        return canUseLongNames() || canUseShortNames();
    }

    public List<List<Function0<? extends String>>> getAllHostNameValues(
            final String otpHome) {
        List<List<Function0<? extends String>>> _xblockexpression = null;
        {
            final Properties p = readErlideHosts();
            final ErlangHostnameRetriever retriever = new ErlangHostnameRetriever(
                    otpHome);
            final Function0<String> _function = () -> p.getProperty("long", "");
            final Function0<String> _function_1 = () -> retriever.getErlangHostName(true);
            final Function0<String> _function_2 = () -> getJavaLongHostName();
            final Function0<String> _function_3 = () -> HostnameChecker.longNameFallback;
            final Function0<String> _function_4 = () -> p.getProperty("short", "");
            final Function0<String> _function_5 = () -> retriever
                    .getErlangHostName(false);
            final Function0<String> _function_6 = () -> getJavaShortHostName();
            final Function0<String> _function_7 = () -> HostnameChecker.shortNameFallback;
            _xblockexpression = Collections
                    .<List<Function0<? extends String>>> unmodifiableList(
                            CollectionLiterals
                                    .<List<Function0<? extends String>>> newArrayList(
                                            Collections
                                                    .<Function0<? extends String>> unmodifiableList(
                                                            CollectionLiterals
                                                                    .<Function0<? extends String>> newArrayList(
                                                                            _function,
                                                                            _function_1,
                                                                            _function_2,
                                                                            _function_3)),
                                            Collections
                                                    .<Function0<? extends String>> unmodifiableList(
                                                            CollectionLiterals
                                                                    .<Function0<? extends String>> newArrayList(
                                                                            _function_4,
                                                                            _function_5,
                                                                            _function_6,
                                                                            _function_7))));
        }
        return _xblockexpression;
    }

    private String findFirstValue(final Iterable<Function0<? extends String>> list,
            final Function1<? super String, ? extends Boolean> predicate) {
        String _xblockexpression = null;
        {
            if (list == null || IterableExtensions.isEmpty(list)) {
                return null;
            }
            final String value = IterableExtensions
                    .<Function0<? extends String>> head(list).apply();
            String _xifexpression = null;
            final Boolean _apply = predicate.apply(value);
            if (_apply.booleanValue()) {
                _xifexpression = value;
            } else {
                _xifexpression = findFirstValue(
                        IterableExtensions.<Function0<? extends String>> tail(list),
                        predicate);
            }
            _xblockexpression = _xifexpression;
        }
        return _xblockexpression;
    }

    private void notifyDeprecatedUsage() {
        if (System.getProperty("erlide.long.name") != null
                || System.getProperty("erlide.short.name") != null) {
            final Job job = new Job("") {
                @Override
                protected IStatus run(final IProgressMonitor monitor) {
                    final int _state = Platform.getBundle("org.erlide.ui").getState();
                    final boolean _tripleNotEquals = _state != Bundle.ACTIVE;
                    if (_tripleNotEquals) {
                        this.schedule(500);
                    } else {
                        final StringConcatenation _builder = new StringConcatenation();
                        _builder.append(
                                "Deprecation notice: the system properties erlide.long.name or erlide.short.name to set the host names to be used by both Erlang and Java.");
                        _builder.newLine();
                        _builder.newLine();
                        _builder.append(
                                "The new way to do that is to edit ~/.erlide.hosts and change the values there if they aren\'t correct.");
                        _builder.newLine();
                        _builder.append(
                                "Please remove the use of the system properties.");
                        MessageReporter.showInfo(_builder.toString());
                    }
                    return Status.OK_STATUS;
                }
            };
            job.schedule(500);
        }
    }

    private boolean loadErlideHosts(final String hostsFileName) {
        boolean _xblockexpression = false;
        {
            final Properties props = new Properties();
            boolean loaded = false;
            try {
                final File f = new File(hostsFileName);
                final FileInputStream is = new FileInputStream(f);
                try {
                    props.load(is);
                    loaded = true;
                } finally {
                    is.close();
                }
            } catch (final Throwable _t) {
                if (_t instanceof Exception) {
                } else {
                    throw Exceptions.sneakyThrow(_t);
                }
            }
            longName = props.getProperty("long", null);
            shortName = props.getProperty("short", null);
            _xblockexpression = loaded && (canUseLongNames() || canUseShortNames());
        }
        return _xblockexpression;
    }

    public Properties readErlideHosts() {
        Properties _xblockexpression = null;
        {
            final Properties props = new Properties();
            try {
                final String _hostsFileName = getHostsFileName();
                final File f = new File(_hostsFileName);
                final FileInputStream is = new FileInputStream(f);
                try {
                    props.load(is);
                } finally {
                    is.close();
                }
            } catch (final Throwable _t) {
                if (_t instanceof Exception) {
                } else {
                    throw Exceptions.sneakyThrow(_t);
                }
            }
            _xblockexpression = props;
        }
        return _xblockexpression;
    }

    public void saveErlideHosts(final String longName, final String shortName) {
        try {
            final Properties props = new Properties();
            props.put("long", longName);
            props.put("short", shortName);
            final String _hostsFileName = getHostsFileName();
            final File f = new File(_hostsFileName);
            final OutputStream out = new FileOutputStream(f);
            try {
                props.store(out, null);
            } finally {
                out.close();
            }
            ErlLogger.debug("  # written to %s", getHostsFileName());
        } catch (final Throwable _t) {
            if (_t instanceof IOException) {
                final IOException e = (IOException) _t;
                e.printStackTrace();
            } else {
                throw Exceptions.sneakyThrow(_t);
            }
        }
    }

    private String getHostsFileName() {
        final StringConcatenation _builder = new StringConcatenation();
        final String _property = System.getProperty("user.home");
        _builder.append(_property);
        _builder.append("/.erlide.hosts");
        return _builder.toString();
    }
}
