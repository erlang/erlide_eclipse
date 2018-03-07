package org.erlide.util

import com.google.common.base.Objects
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStream
import java.net.InetAddress
import java.net.UnknownHostException
import java.util.List
import java.util.Properties
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Platform
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.jobs.Job
import org.osgi.framework.Bundle

class HostnameChecker {
    static val longNameFallback = "127.0.0.1"
    static val shortNameFallback = "localhost"

    String longName
    String shortName

    private new() {
    }

    private static class Holder {
        static final HostnameChecker INSTANCE = new HostnameChecker()
    }

    def static HostnameChecker getInstance() {
        return Holder.INSTANCE
    }

    def String getErlangHostName(boolean useLongName) {
        if(useLongName) longName else shortName
    }

    def boolean isThisHost(String host) {
        Objects.equal(host, longName) || Objects.equal(host, shortName)
    }

    def boolean canUseLongNames() {
        longName !== null
    }

    def boolean canUseShortNames() {
        shortName !== null
    }

    def String getJavaLongHostName() {
        var String name = null
        try {
            val InetAddress addr = InetAddress.getLocalHost()
            name = addr.getCanonicalHostName()
            if (!name.contains("."))
                name = null
        } catch (UnknownHostException e1) {
            // ignore
        }
        name
    }

    def String getJavaShortHostName() {
        var String name = null
        try {
            val InetAddress addr = InetAddress.getLocalHost()
            name = addr.getHostName()
            if (name.contains("."))
                name = null
        } catch (UnknownHostException e1) {
            // ignore
        }
        name
    }

    /**
     * Start erlang nodes and find out how they resolve the long/short host names.
     */
    def boolean detectHostNames(String otpHome) {
        notifyDeprecatedUsage()

        val ErlangHostnameRetriever retriever = new ErlangHostnameRetriever(otpHome)

        val loadProperties = getErlideHostsFromProperties()
        if(loadProperties) {
            // if user uses this, we assume he knows best and don't check connectivity here
            return true
        }
        val loaded = loadErlideHosts(hostsFileName)
        if (loaded) {
            if (retriever.canConnect(shortName, false) || retriever.canConnect(longName, true))
                return true
        }

        val Iterable<()=>String> longValues = #[
            [|longName],
            [|retriever.getErlangHostName(true)],
            [|javaLongHostName],
            [|longNameFallback]
        ]
        longName = findFirstValue(longValues)[it !== null && retriever.canConnect(it, true)]

        val Iterable<()=>String> shortValues = #[
            [|shortName],
            [|retriever.getErlangHostName(false)],
            [|javaShortHostName],
            [|shortNameFallback]
        ]
        shortName = findFirstValue(shortValues)[it !== null && retriever.canConnect(it, false)]

        ErlLogger.debug("Detected:: '%s' && '%s'", shortName, longName)
        return canUseLongNames || canUseShortNames
    }

    def getErlideHostsFromProperties() {
        shortName = System.getProperty("erlide.host.short")
        longName = System.getProperty("erlide.host.long")
        return canUseLongNames || canUseShortNames
    }

    def List<List<()=>String>> getAllHostNameValues(String otpHome) {
        val p = readErlideHosts()
        val ErlangHostnameRetriever retriever = new ErlangHostnameRetriever(otpHome)
        #[
            #[
				[|p.getProperty("long", "")], [|retriever.getErlangHostName(true)], [|javaLongHostName], [|
                longNameFallback
            ]],
            #[
				[|p.getProperty("short", "")], [|retriever.getErlangHostName(false)], [|javaShortHostName], [|
                shortNameFallback
            ]]
        ]
    }

    def private String findFirstValue(Iterable<()=>String> list, (String)=>boolean predicate) {
        if(list === null || list.empty) return null

        val value = list.head.apply
        if (predicate.apply(value))
            value
        else
            findFirstValue(list.tail, predicate)
    }

    def private void notifyDeprecatedUsage() {
        if (System.getProperty("erlide.long.name") !== null || System.getProperty("erlide.short.name") !== null) {
            val Job job = new Job("") {

                override protected run(IProgressMonitor monitor) {
                    if (Platform.getBundle("org.erlide.ui").getState() !== Bundle.ACTIVE) {
                        schedule(500)
                    } else {
                        MessageReporter.showInfo('''
                        Deprecation notice: the system properties erlide.long.name or erlide.short.name to set the host names to be used by both Erlang and Java.

                        The new way to do that is to edit ~/.erlide.hosts and change the values there if they aren't correct.
                        Please remove the use of the system properties.''')
                    }
                    return Status.OK_STATUS
                }
            } as Job
            job.schedule(500)
        }

    }

    def private boolean loadErlideHosts(String hostsFileName) {
        val Properties props = new Properties()
        var loaded = false
        try {
            val File f = new File(hostsFileName)
            val FileInputStream is = new FileInputStream(f)
            try {
                props.load(is)
                loaded = true
            } finally {
                is.close()
            }
        } catch (Exception e) {
        }
        longName = props.getProperty("long", null)
        shortName = props.getProperty("short", null)
        loaded && (canUseLongNames || canUseShortNames)
    }

    def public Properties readErlideHosts() {
        val Properties props = new Properties()
        var loaded = false
        try {
            val File f = new File(hostsFileName)
            val FileInputStream is = new FileInputStream(f)
            try {
                props.load(is)
                loaded = true
            } finally {
                is.close()
            }
        } catch (Exception e) {
        }
        props
    }

    def public void saveErlideHosts(String longName, String shortName) {
        try {
            val Properties props = new Properties()
            props.put("long", longName)
            props.put("short", shortName)
            val File f = new File(hostsFileName)
            val OutputStream out = new FileOutputStream(f)
            try {
                props.store(out, null)
            } finally {
                out.close()
            }
            ErlLogger.debug("  # written to %s", hostsFileName)
        } catch (IOException e) {
            e.printStackTrace()
        }
    }

    def private String getHostsFileName() {
        '''«System.getProperty("user.home")»/.erlide.hosts'''
    }
}
