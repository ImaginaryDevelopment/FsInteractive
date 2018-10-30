using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static CodeGeneration.GenerateAllTheThings;
using Newtonsoft.Json;

namespace CodeGeneration.UI
{
    // perhaps one day, make it able to import other types besides just dataModels
    public partial class UcJsonImport : UserControl
    {
        TableInput[] _tableInputs;
        public IEnumerable<TableInput> TableInputs => _tableInputs;

        public UcJsonImport()
        {
            InitializeComponent();
        }

        void btnImport_Click(object sender, EventArgs e)
        {
            if (string.IsNullOrWhiteSpace(this.textBox1.Text))
                return;
            try
            {
                _tableInputs = JsonConvert.DeserializeObject<TableInput[]>(this.textBox1.Text);
            }
            catch(Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
            this.textBox1.Text = JsonConvert.SerializeObject(_tableInputs, Formatting.Indented);
            this.btnImport.Enabled = false;
        }

        void textBox1_TextChanged(object sender, EventArgs e)
        {
            btnImport.Enabled = true;
        }
    }
}
