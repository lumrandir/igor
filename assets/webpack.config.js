const path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const OptimizeCSSAssetsPlugin = require('css-minimizer-webpack-plugin');

module.exports = {
  optimization: {
    minimizer: [
      new OptimizeCSSAssetsPlugin({})
    ]
  },
  entry: './js/app.js',
  output: {
    path: path.resolve(__dirname, '../static/js'),
    filename: 'app.js'
  },
  module: {
    rules: [
      {
        test: /\.[s]?css$/,
        use: [
          MiniCssExtractPlugin.loader,
          'css-loader',
          'sass-loader'
        ]
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {
            cwd: path.resolve(__dirname, 'elm'),
            debug: false,
            optimize: true
          }
        }
      }
    ]
  },
  plugins: [
    new MiniCssExtractPlugin({ filename: '../css/app.css' })
  ]
}
