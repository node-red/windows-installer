[Code]

// nodedetector.iss
function detect_red_installations(var page: TOutputMarqueeProgressWizardPage): integer; forward;
function red_list(working_dir: string): string; forward;

// redactionpage.iss
function MakeRedActionPage(page: TInputOptionWizardPage): Boolean; forward;
