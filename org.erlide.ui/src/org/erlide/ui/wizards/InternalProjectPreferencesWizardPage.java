package org.erlide.ui.wizards;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wb.swt.SWTResourceManager;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.util.PreferencesUtils;
import org.erlide.util.SystemConfiguration;

import com.google.common.collect.Lists;

public class InternalProjectPreferencesWizardPage extends ProjectPreferencesWizardPage {

    Text externalModules;
    Text externalIncludes;
    private Button discoverBtn;
    private Button externalModulesBrowse;
    private Button externalIncludesBrowse;

    public InternalProjectPreferencesWizardPage(final String pageName,
            final NewProjectData info) {
        super(pageName, info);
        configType = ProjectConfigType.INTERNAL;
    }

    /**
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createControl(final Composite parent) {
        super.createControl(parent);
        final Composite composite = (Composite) getControl();

        output.setEnabled(true);
        source.setEnabled(true);
        include.setEnabled(true);
        test.setEnabled(false); // TODO fix when testing works

        discoverBtn = new Button(composite, SWT.PUSH);
        FormData fd_discoverBtn;
        {
            fd_discoverBtn = new FormData();
            fd_discoverBtn.bottom = new FormAttachment(100, -96);
            fd_discoverBtn.right = new FormAttachment(100, -10);
            discoverBtn.setLayoutData(fd_discoverBtn);
        }
        discoverBtn.setToolTipText("Tries to guess the project's configuration \n"
                + "by finding all erl and hrl files");
        discoverBtn.setText("Discover paths...");
        discoverBtn.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(final Event event) {
                discoverPaths();
            }
        });

        if (SystemConfiguration.getInstance().isTest()) {
            createExternalModuleEditor(composite);
            createExternalIncludeEditor(composite);
        }

    }

    protected void discoverPaths() {
        if (info.isExistingProject()) {
            final IPath loc = info.getLocation();
            final File dir = loc.toFile();

            final List<String> src = search("erl", dir);
            if (src.contains("test")) {
                src.remove("test");
                final List<String> tst = Lists.newArrayList("test");
                final List<String> tsts = dirs(tst, loc);
                test.setText(PreferencesUtils.packList(tsts));
            }
            final List<String> srcs = dirs(src, loc);
            if (loc.append("src").toFile().exists() && !srcs.contains("src")) {
                srcs.add("src");
            }

            final List<String> inc = search("hrl", dir);
            final List<String> incs = dirs(inc, loc);
            if (loc.append("include").toFile().exists() && !incs.contains("include")) {
                incs.add("include");
            }

            source.setText(PreferencesUtils.packList(srcs));
            include.setText(PreferencesUtils.packList(incs));
        }
    }

    private List<String> dirs(final List<String> list, final IPath ref) {
        final int n = ref.segmentCount();
        final List<String> result = Lists.newArrayList();
        for (final Iterator<String> iter = list.iterator(); iter.hasNext();) {
            final String element = iter.next();
            IPath p = new Path(element);
            p = p.removeLastSegments(1).removeFirstSegments(n).setDevice(null);
            String ps = p.toString();
            if ("".equals(ps)) {
                ps = ".";
            }
            if (result.indexOf(ps) < 0) {
                result.add(ps);
            }
        }
        return result;
    }

    private List<String> search(final String ext, final File file) {
        return search(ext, file, new ArrayList<String>());
    }

    private List<String> search(final String ext, final File file, final List<String> list) {
        if (file.isFile()) {
            final IPath path = new Path(file.getPath());
            if (path.getFileExtension() != null && path.getFileExtension().equals(ext)) {
                list.add(file.getPath());
            }
        } else if (file.isDirectory()) {
            final File[] fs = file.listFiles();
            for (final File f : fs) {
                search(ext, f, list);
            }
        }
        return list;
    }

    private void createExternalModuleEditor(final Composite parent) {
        final Composite composite = parent;

        final String resourceString4 = "External modules file";
        final Label label = new Label(composite, SWT.NONE);
        {
            final FormData fd_label = new FormData();
            fd_label.top = new FormAttachment(0, 207);
            fd_label.left = new FormAttachment(0, 5);
            label.setLayoutData(fd_label);
        }
        label.setBackground(SWTResourceManager.getColor(255, 255, 183));
        label.setText(resourceString4 + ":");
        externalModules = new Text(composite, SWT.BORDER);
        {
            final FormData fd_externalModules = new FormData();
            fd_externalModules.right = new FormAttachment(0, 477);
            fd_externalModules.top = new FormAttachment(0, 202);
            fd_externalModules.left = new FormAttachment(0, 141);
            externalModules.setLayoutData(fd_externalModules);
        }
        externalModules.setToolTipText("enter a list of folders");
        externalModules.setText(info.getExternalModulesFile());
        externalModules.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                info.setExternalModulesFile(externalModules.getText());
            }
        });
        externalModulesBrowse = new Button(composite, SWT.NONE);
        {
            final FormData fd_externalModulesBrowse = new FormData();
            fd_externalModulesBrowse.top = new FormAttachment(0, 203);
            fd_externalModulesBrowse.left = new FormAttachment(0, 482);
            externalModulesBrowse.setLayoutData(fd_externalModulesBrowse);
        }
        externalModulesBrowse.setText("Browse...");
        externalModulesBrowse.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent evt) {
                handleExternalModulesBrowseSelected();
            }

        });
    }

    private void createExternalIncludeEditor(final Composite parent) {
        final Composite composite = parent;

        final String resourceString4 = "External includes file";
        final Label label = new Label(composite, SWT.NONE);
        {
            final FormData fd_label = new FormData();
            fd_label.top = new FormAttachment(0, 237);
            fd_label.left = new FormAttachment(0, 5);
            label.setLayoutData(fd_label);
        }
        label.setBackground(SWTResourceManager.getColor(255, 255, 183));
        label.setText(resourceString4 + ":");
        externalIncludes = new Text(composite, SWT.BORDER);
        {
            final FormData fd_externalIncludes = new FormData();
            fd_externalIncludes.right = new FormAttachment(0, 477);
            fd_externalIncludes.top = new FormAttachment(0, 232);
            fd_externalIncludes.left = new FormAttachment(0, 141);
            externalIncludes.setLayoutData(fd_externalIncludes);
        }
        externalIncludes.setToolTipText("enter a list of folders");
        externalIncludes.setText(info.getExternalIncludesFile());
        externalIncludes.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                info.setExternalIncludesFile(externalIncludes.getText());
            }
        });
        externalIncludesBrowse = new Button(composite, SWT.NONE);
        {
            final FormData fd_externalIncludesBrowse = new FormData();
            fd_externalIncludesBrowse.top = new FormAttachment(0, 233);
            fd_externalIncludesBrowse.left = new FormAttachment(0, 482);
            externalIncludesBrowse.setLayoutData(fd_externalIncludesBrowse);
        }
        externalIncludesBrowse.setText("Browse...");
        externalIncludesBrowse.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent evt) {
                handleExternalIncludesBrowseSelected();
            }

        });
    }

    protected void handleExternalModulesBrowseSelected() {
        String last = externalModules.getText();
        if (last == null) {
            last = ""; //$NON-NLS-1$
        } else {
            last = last.trim();
        }
        final FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);
        dialog.setText("Select file with external modules");
        dialog.setFileName(last);
        dialog.setFilterExtensions(new String[] { "*.erlidex" });
        final String result = dialog.open();
        if (result == null) {
            return;
        }
        externalModules.setText(result);
    }

    protected void handleExternalIncludesBrowseSelected() {
        String last = externalIncludes.getText();
        if (last == null) {
            last = ""; //$NON-NLS-1$
        } else {
            last = last.trim();
        }
        final FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);
        dialog.setText("Select file with external include files");
        dialog.setFileName(last);
        dialog.setFilterExtensions(new String[] { "*.erlidex" });
        final String result = dialog.open();
        if (result == null) {
            return;
        }
        externalIncludes.setText(result);
    }

    @Override
    protected String getBuilderDescription() {
        return "Configuration stored in the project preferences.";
    }

    @Override
    protected void onEntry() {
        super.onEntry();
        discoverBtn.setEnabled(info.isExistingProject());
    }

    @Override
    protected void onExit() {
    }

}
