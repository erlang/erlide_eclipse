package org.erlide.ui.editors.erl.correction;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.builder.ProblemData;
import org.erlide.engine.model.erlang.IErlModule;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;

//TODO make this an engine service?

public class ErlangQuickFixCollector {

    private static List<ErlangQuickFix> fixes;

    public ErlangQuickFixCollector() {
        if (fixes == null) {
            fixes = getQuickFixes();
        }
    }

    public IMarkerResolution[] getFixes(final IMarker marker) {
        final IResource resource = marker.getResource();
        final IErlModel model = ErlangEngine.getInstance().getModel();
        IErlModule module;

        if (resource == null) {
            return new IMarkerResolution[0];
        }
        if (resource instanceof IProject) {
            return getFixesForProject((IProject) resource, marker);

        }
        if (resource instanceof IFile) {
            module = model.findModule((IFile) resource);
            if (module != null) {
                return getFixesForModule(module, marker);
            }
        }

        return new IMarkerResolution[0];
    }

    private IMarkerResolution[] getFixesForProject(final IProject project,
            final IMarker marker) {
        return new IMarkerResolution[0];
    }

    private IMarkerResolution[] getFixesForModule(final IErlModule module,
            final IMarker marker) {
        final List<IMarkerResolution> result = Lists.newArrayList();

        final String tag = marker.getAttribute(ProblemData.TAG, "");
        final String strArgs = marker.getAttribute(ProblemData.ARGS, "");
        final List<String> args = Splitter.on('\0').splitToList(strArgs);

        for (final ErlangQuickFix fix : fixes) {
            if (fix.getTags().contains(tag)) {
                final ErlangQuickFix myfix = new ErlangQuickFix(fix);
                myfix.setArgs(getMyArgs(myfix.getLabel(), args));
                myfix.setLabel(formatLabel(myfix.getLabel(), args));
                result.add(myfix);
            }
        }
        return result.toArray(new IMarkerResolution[result.size()]);
    }

    private String formatLabel(final String label, final List<String> args) {
        String result = label;
        int i = 0;
        for (final String arg : args) {
            result = result.replace("{" + i + "}", arg);
            i++;
        }
        return result;
    }

    private List<String> getMyArgs(final String label, final List<String> args) {
        final List<String> result = Lists.newArrayList();
        final Pattern rexp = Pattern.compile("\\{([0-9])}");
        final Matcher matcher = rexp.matcher(label);
        while (matcher.find()) {
            String num = matcher.group();
            num = num.substring(1, num.length() - 1);
            result.add(args.get(Integer.parseInt(num)));
        }
        return result;
    }

    private List<ErlangQuickFix> getQuickFixes() {
        final List<ErlangQuickFix> result = Lists.newArrayList();
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor("org.erlide.ui.quickfix");
        for (final IConfigurationElement element : elements) {
            final ErlangQuickFix qf = new ErlangQuickFix();
            qf.setLabel(element.getAttribute("label"));
            qf.setDescription(element.getAttribute("description"));
            qf.setImage(mkImage(element.getAttribute("image")));
            qf.setTags(getTags(element));
            qf.setConfigurationElement(element);
            result.add(qf);
        }
        return result;
    }

    private Image mkImage(final String path) {
        if (path == null) {
            return null;
        }
        final ImageDescriptor imageDescription = AbstractUIPlugin
                .imageDescriptorFromPlugin("org.erlide.ui", path);
        return imageDescription.createImage();
    }

    private List<String> getTags(final IConfigurationElement element) {
        final List<String> result = Lists.newArrayList();
        for (final IConfigurationElement tagElement : element.getChildren()) {
            result.add(tagElement.getAttribute("name"));
        }
        return result;
    }

}
