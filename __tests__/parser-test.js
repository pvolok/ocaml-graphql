const fs = require('fs');
const path = require('path');
const proc = require('child_process');

describe('parser', () => {
  let files = fs.readdirSync(__dirname);
  files = files.filter(file => file.endsWith('.graphql'));
  for (const file of files) {
    it('Parse ' + file, () => {
      const ast = proc.execSync(
        `./print_ast.byte __tests__/${file}`,
        {encoding: 'utf8'},
      );
      expect(ast).toMatchSnapshot();
    });
  }
});
