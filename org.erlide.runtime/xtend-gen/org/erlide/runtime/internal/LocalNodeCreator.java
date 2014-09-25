package org.erlide.runtime.internal;

import com.ericsson.otp.erlang.OtpNode;
import com.google.common.base.Strings;
import java.io.IOException;
import java.net.Socket;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.runtime.OtpNodeProxy;
import org.erlide.runtime.internal.ErlideNodeStatus;
import org.erlide.runtime.internal.NodeNameCreator;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameUtils;
import org.fishwife.jrugged.Initializable;
import org.fishwife.jrugged.Initializer;

@SuppressWarnings("all")
public class LocalNodeCreator {
  public static OtpNode createOtpNode(final String cookie, final boolean longName) {
    try {
      OtpNode _xblockexpression = null;
      {
        final String hostName = HostnameUtils.getErlangHostName(longName);
        OtpNode _xifexpression = null;
        boolean _isNullOrEmpty = Strings.isNullOrEmpty(cookie);
        if (_isNullOrEmpty) {
          String _create = NodeNameCreator.create(hostName);
          _xifexpression = new OtpNode(_create);
        } else {
          String _create_1 = NodeNameCreator.create(hostName);
          _xifexpression = new OtpNode(_create_1, cookie);
        }
        _xblockexpression = _xifexpression;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static OtpNode startLocalNode(final OtpNodeProxy runtime, final String cookie, final boolean hasLongName) {
    LocalNodeCreator.wait_for_epmd();
    final OtpNode lNode = LocalNodeCreator.createOtpNode(cookie, hasLongName);
    final ErlideNodeStatus statusWatcher = new ErlideNodeStatus(runtime);
    lNode.registerStatusHandler(statusWatcher);
    return lNode;
  }
  
  public static void wait_for_epmd() {
    LocalNodeCreator.wait_for_epmd(null);
  }
  
  private final static int EPMD_PORT = Integer.parseInt(System.getProperty("erlide.epmd.port", "4369"));
  
  public final static long POLL_INTERVAL = 100;
  
  public static void wait_for_epmd(final String host) {
    final Initializable client = new Initializable() {
      public void afterInit() {
      }
      
      public void configuredRetriesMetOrExceededWithoutSuccess() {
        StringConcatenation _builder = new StringConcatenation();
        _builder.append("Couldn\'t contact epmd - erlang backend is probably not working.");
        _builder.newLine();
        _builder.append("Your host\'s entry in /etc/hosts is probably wrong (");
        _builder.append(host, "");
        _builder.append(").");
        _builder.newLineIfNotEmpty();
        final String msg = _builder.toString();
        ErlLogger.error(msg);
        throw new RuntimeException(msg);
      }
      
      public void tryInit() throws Exception {
        try {
          final Socket s = new Socket(host, LocalNodeCreator.EPMD_PORT);
          s.close();
        } catch (final Throwable _t) {
          if (_t instanceof IOException) {
            final IOException e = (IOException)_t;
          } else {
            throw Exceptions.sneakyThrow(_t);
          }
        }
      }
    };
    final Initializer initializer = new Initializer(client);
    initializer.setMaxRetries(30);
    initializer.setRetryMillis(LocalNodeCreator.POLL_INTERVAL);
    initializer.run();
  }
}
