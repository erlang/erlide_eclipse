package org.erlide.cover.core;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.cover.api.CoverException;
import org.erlide.cover.api.IConfiguration;
import org.erlide.cover.api.ICoveragePerformer;
import org.erlide.cover.constants.CoverConstants;
import org.erlide.cover.views.model.StatsTreeModel;
import org.erlide.jinterface.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Implementation of coverage analysis operations
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class CoveragePerformer implements ICoveragePerformer {

    private static CoveragePerformer performer;

    private Collection<String> coverNodes;
    private IConfiguration config;

    private final Logger log; // logger

    private CoveragePerformer() {
        log = Activator.getDefault();
        coverNodes = new LinkedList<String>();
    }

    public static synchronized CoveragePerformer getPerformer() {
        if (performer == null) {
            performer = new CoveragePerformer();
        }
        return performer;
    }

    /**
     * Start cover
     */
    @Override
    public synchronized void startCover(final Collection<String> nodes)
            throws CoverException {

        final StatsTreeModel model = StatsTreeModel.getInstance();
        model.clear();
        if (CoverBackend.getInstance().getAnnotationMaker() != null) {
            CoverBackend.getInstance().getAnnotationMaker()
                    .clearAllAnnotations();
        }

        for (final ICoverObserver obs : CoverBackend.getInstance()
                .getListeners()) {
            obs.eventOccured(new CoverEvent(CoverStatus.UPDATE));
        }

        boolean different = false;
        for (final String node : nodes) {
            if (!coverNodes.contains(node)) {
                different = true;
                break;
            }
        }

        if (coverNodes.isEmpty() || different) {

            coverNodes = nodes;
            log.info(CoverBackend.getInstance().getBackend().getFullNodeName());
            coverNodes.add(CoverBackend.getInstance().getBackend()
                    .getFullNodeName());

            // TODO restarting

            final List<OtpErlangObject> names = new ArrayList<OtpErlangObject>(
                    coverNodes.size());
            for (final String name : coverNodes) {
                names.add(new OtpErlangAtom(name));
            }

            final OtpErlangList nodesList = new OtpErlangList(
                    names.toArray(new OtpErlangObject[0]));

            try {
                CoverBackend
                        .getInstance()
                        .getBackend()
                        .call(CoverConstants.COVER_ERL_BACKEND,
                                CoverConstants.FUN_START, "x", nodesList);

            } catch (final RpcException e) {
                e.printStackTrace();
                throw new CoverException(e.getMessage());
            }

        }

    }

    /**
     * Set coverage configuration
     */
    @Override
    public synchronized void setCoverageConfiguration(final IConfiguration conf)
            throws CoverException {
        config = conf;

        StatsTreeModel.getInstance()
                .setRootLabel(config.getProject().getName());

        final IPath ppath = config.getProject().getWorkspaceProject()
                .getLocation();

        // set include files
        final List<OtpErlangObject> includes = new ArrayList<OtpErlangObject>(
                config.getModules().size());
        for (final IPath include : config.getIncludeDirs()) {
            log.info(ppath.append(include));
            includes.add(new OtpErlangList(ppath.append(include).toString()));
        }

        try {
            CoverBackend
                    .getInstance()
                    .getBackend()
                    .call(CoverConstants.COVER_ERL_BACKEND,
                            CoverConstants.FUN_SET_INCLUDES, "x", includes);
        } catch (final RpcException e1) {
            e1.printStackTrace();
            throw new CoverException(e1);
        }

        recompileModules();
    }

    // cover compilation of chosen modules
    private void recompileModules() throws CoverException {
        final List<OtpErlangObject> paths = new ArrayList<OtpErlangObject>(
                config.getModules().size());
        for (final IErlModule module : config.getModules()) {
            if (module == null) {
                final String msg = "No such module at given project. Check your configuration";
                CoverBackend.getInstance().handleError(msg);
                throw new CoverException(msg);
            }
            log.info(module.getFilePath());
            paths.add(new OtpErlangList(module.getFilePath()));
        }

        try {
            CoverBackend
                    .getInstance()
                    .getBackend()
                    .call(CoverConstants.COVER_ERL_BACKEND,
                            CoverConstants.FUN_PREP, "x", paths);

        } catch (final RpcException e) {
            e.printStackTrace();
            throw new CoverException(e.getMessage());
        }
    }

    /**
     * Perform coverage analysis
     */
    @Override
    public synchronized void analyse() throws CoverException {

        final List<OtpErlangObject> modules = new ArrayList<OtpErlangObject>(
                config.getModules().size());
        for (final IErlModule module : config.getModules()) {
            log.info(module.getModuleName());
            modules.add(new OtpErlangList(module.getModuleName()));
        }

        try {
            CoverBackend
                    .getInstance()
                    .getBackend()
                    .call(CoverConstants.COVER_ERL_BACKEND,
                            CoverConstants.FUN_ANALYSE, "x", modules);

        } catch (final RpcException e) {
            e.printStackTrace();
            throw new CoverException(e.getMessage());
        }
    }

    /**
     * Allows to check configuration
     */
    @Override
    public IConfiguration getConfig() {
        return config;
    }

}
