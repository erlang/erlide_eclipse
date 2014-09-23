package org.erlide.runtime.internal;

import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;
import com.google.common.base.Strings;
import java.io.IOException;
import java.net.Socket;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.runtime.internal.ErlRuntime;
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
      OtpNode node = null;
      final String hostName = HostnameUtils.getErlangHostName(longName);
      boolean _isNullOrEmpty = Strings.isNullOrEmpty(cookie);
      if (_isNullOrEmpty) {
        String _create = NodeNameCreator.create(hostName);
        OtpNode _otpNode = new OtpNode(_create);
        node = _otpNode;
      } else {
        String _create_1 = NodeNameCreator.create(hostName);
        OtpNode _otpNode_1 = new OtpNode(_create_1, cookie);
        node = _otpNode_1;
      }
      return node;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static OtpNode startLocalNode(final ErlRuntime runtime, final String cookie, final boolean hasLongName) {
    LocalNodeCreator.wait_for_epmd();
    final OtpNode lNode = LocalNodeCreator.createOtpNode(cookie, hasLongName);
    final OtpNodeStatus statusWatcher = new ErlideNodeStatus(runtime);
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
        final String msg = ((("Couldn\'t contact epmd - erlang backend is probably not working\n" + 
          "Your host\'s entry in /etc/hosts is probably wrong (") + host) + ").");
        ErlLogger.error(msg);
        throw new RuntimeException(msg);
      }
      
      public void tryInit() throws Exception {
        Socket s = null;
        try {
          Socket _socket = new Socket(host, LocalNodeCreator.EPMD_PORT);
          s = _socket;
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
    initializer.initialize();
  }
}
