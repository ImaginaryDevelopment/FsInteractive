using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static CodeGeneration.DataModelToF;
using Microsoft.FSharp.Collections;
using BCore.CHelpers;

namespace CodeGeneration.UI
{
    public partial class UcSettings : UserControl
    {
        // consider making these private once you have the generate settings method
        public string TargetProjectName => txtTargetProjectName.Text;
        public string TargetNamespace => txtTargetNamespace.Text;
        public bool UseOptionTypes => cbUseOptionTypes.Checked;
        public bool IncludeNonDboSchemaInNamespace => cbIncludeNonDboSchemaInNamespace.Checked;

        public UcSettings()
        {
            InitializeComponent();
        }

        FSharpMap<TKey, TValue> MakeMap<TKey, TValue>()
            => MapModule.Empty<TKey, TValue>();
        public CodeGenSettingMap GenerateSettingsMap()
        {
            var pluralService = System.Data.Entity.Design.PluralizationServices.PluralizationService.CreateService(System.Globalization.CultureInfo.CurrentUICulture);
            var emptyStringSet = SetModule.Empty<string>();
            return new CodeGenSettingMap(
                targetProjectName: this.TargetProjectName,
                targetNamespace: this.TargetNamespace,
                typeScriptGenSettingMap: null,
                cString: null,
                nullableHandling: this.UseOptionTypes ?
                    PureCodeGeneration.NullableHandling.UseOptions
                    : PureCodeGeneration.NullableHandling.UseNullable,
                columnNolist: MapModule.Empty<string, FSharpSet<string>>(),
                typeGenerationNolist: emptyStringSet,
                measures: emptyStringSet,
                measuresNolist: emptyStringSet,
                additionalNamespaces: emptyStringSet,
                getMeasureNamepace: null,
                pluralize: Delegates.toFSharpFunc<string, string>(pluralService.Pluralize),
                singularize: Delegates.toFSharpFunc<string, string>(pluralService.Singularize),
                includeNonDboSchemaInNamespace: cbIncludeNonDboSchemaInNamespace.Checked,
                generateValueRecords: false,
                sprocSettingMap: null,
                mutable: PureCodeGeneration.Mutability.Immutable
                );
        }

    }
}
