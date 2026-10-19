// Sem dependências externas; opera apenas as tabelas locais do relatório.
(() => {
  const visibleRows = table => [...table.tBodies[0].rows].filter(r => !r.hidden);
  const headers = table => [...table.tHead.rows[0].cells].map(c => c.textContent.trim());
  const values = row => [...row.cells].map(c => c.dataset.type === 'number'
    ? (c.dataset.value === '' ? null : Number(c.dataset.value)) : c.dataset.value);
  const quote = value => '"' + String(value ?? '').replaceAll('"', '""') + '"';
  const delimited = (table, sep) => [headers(table), ...visibleRows(table).map(values)]
    .map(row => row.map(v => quote(typeof v === 'number' && sep === ';'
      ? String(v).replace('.', ',') : v)).join(sep)).join('\r\n');
  const save = (data, name, type) => {
    const url = URL.createObjectURL(new Blob([data], {type}));
    const link = document.createElement('a'); link.href = url; link.download = name;
    document.body.append(link); link.click(); link.remove();
    setTimeout(() => URL.revokeObjectURL(url), 1500);
  };
  for (const section of document.querySelectorAll('details.dados')) {
    const table = section.querySelector('table');
    const count = section.querySelector('.contagem');
    const update = () => count.textContent = `${visibleRows(table).length} de ${table.tBodies[0].rows.length} linhas`;
    update();
    section.querySelector('.busca').addEventListener('input', event => {
      const q = event.target.value.toLocaleLowerCase('pt-BR').trim();
      for (const row of table.tBodies[0].rows) row.hidden = ![...row.cells].map(c=>c.textContent).join(' ').toLocaleLowerCase('pt-BR').includes(q);
      update();
    });
    const sort = th => {
      const column = Number(th.dataset.col), direction = th.getAttribute('aria-sort') === 'ascending' ? -1 : 1;
      const rows = [...table.tBodies[0].rows];
      rows.sort((a,b) => {
        const x = a.cells[column], y = b.cells[column];
        if (x.dataset.value === '') return y.dataset.value === '' ? 0 : 1;
        if (y.dataset.value === '') return -1;
        return direction * (x.dataset.type === 'number'
          ? Number(x.dataset.value) - Number(y.dataset.value)
          : x.dataset.value.localeCompare(y.dataset.value, 'pt-BR', {numeric:true}));
      });
      for (const h of table.tHead.rows[0].cells) h.setAttribute('aria-sort', 'none');
      th.setAttribute('aria-sort', direction === 1 ? 'ascending' : 'descending');
      table.tBodies[0].append(...rows);
    };
    for (const th of table.tHead.rows[0].cells) {
      th.addEventListener('click', () => sort(th));
      th.addEventListener('keydown', e => {if (e.key === 'Enter' || e.key === ' ') {e.preventDefault();sort(th);}});
    }
    for (const button of section.querySelectorAll('button[data-action]')) button.addEventListener('click', async () => {
      const action = button.dataset.action, name = table.dataset.export;
      if (action === 'csv') save('\uFEFF' + delimited(table, ';'), name + '.csv', 'text/csv;charset=utf-8');
      if (action === 'tsv') save('\uFEFF' + delimited(table, '\t'), name + '.tsv', 'text/tab-separated-values;charset=utf-8');
      if (action === 'json') {
        const h = headers(table), records = visibleRows(table).map(row => Object.fromEntries(values(row).map((v,i)=>[h[i],v])));
        save(JSON.stringify(records, null, 2), name + '.json', 'application/json;charset=utf-8');
      }
      if (action === 'copy') {
        const text = delimited(table, '\t');
        try {await navigator.clipboard.writeText(text);} catch {
          const area = document.createElement('textarea'); area.value = text; document.body.append(area); area.select(); document.execCommand('copy'); area.remove();
        }
        count.textContent = 'Tabela copiada';
      }
      if (action === 'print') {
        const popup = window.open('', '_blank');
        if (!popup) {count.textContent = 'Permita a abertura da janela de impressão';return;}
        const copy = table.cloneNode(true);for(const row of [...copy.tBodies[0].rows])if(row.hidden)row.remove();
        const title = section.querySelector('summary').textContent;
        popup.document.write('<!doctype html><html lang="pt-BR"><meta charset="utf-8"><title>Exportar tabela</title><style>@page{size:A4 landscape;margin:12mm}body{font:10px Arial}table{border-collapse:collapse;width:100%}td,th{border:1px solid #ccd;padding:5px}thead{display:table-header-group}tr{break-inside:avoid}</style><body><h1></h1></body></html>');
        popup.document.close();popup.document.querySelector('h1').textContent = title;popup.document.body.append(copy);
        popup.focus();setTimeout(() => popup.print(), 150);
      }
    });
  }
})();
