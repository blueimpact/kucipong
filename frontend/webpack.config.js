// Thanks to [moarwick](https://github.com/moarwick/elm-webpack-starter)

const path              = require('path');
const webpack           = require('webpack');
const merge             = require('webpack-merge');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const autoprefixer      = require('autoprefixer');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

console.log('Start Webpack process...');

// Determine build env by npm command options
const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? 'production' : 'development';
const ENV = TARGET_ENV === 'production' ?
  {
    'apiRoot': JSON.stringify(
      process.env.API_ROOT || 'http://kucipong.com/api/v0/'
    ),
  } :
  {
    'apiRoot': JSON.stringify(
      process.env.API_ROOT || 'http://localhost:8081/v0/'
    ),
  };

// Common webpack config
const commonConfig = {

  // Directory to output compiled files
  output: {
    path: path.resolve(__dirname, 'dist/'),
    filename: '[name]-[hash].js',
  },

  resolve: {
    modulesDirectories: ['node_modules'],
    extensions: ['', '.js', '.elm'],
  },

  module: {
    noParse: /\.elm$/,
    loaders: [
      {
        test: /\.(eot|ttf|woff|woff2|svg)$/,
        loader: 'file-loader',
      },
      {
        test: /\.pug$/,
        loader: 'pug',
      },
      {
        test: /\.(jpg|jpeg|png)$/,
        loader: 'url'
      },
    ]
  },

  plugins: [
    // Compile chat page
    new HtmlWebpackPlugin({
      chunks: ['chat'],
      template: 'src/pug/chat.pug',
      inject:   'body',
      filename: 'chat.html',
    }),
    // Compile end-user related pages
    new HtmlWebpackPlugin({
      chunks: ['endUser'],
      template: 'src/pug/endUser_coupon_id.pug',
      inject:   'body',
      filename: 'endUser_coupon_id.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['endUser'],
      template: 'src/pug/endUser_store_id.pug',
      inject:   'body',
      filename: 'endUser_store_id.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['endUser'],
      template: 'src/pug/endUser_store_id_coupon.pug',
      inject:   'body',
      filename: 'endUser_store_id_coupon.html',
    }),

    // Compile store-user related pages
    new HtmlWebpackPlugin({
      chunks: ['storeUser'],
      template: 'src/pug/storeUser_store.pug',
      inject:   'body',
      filename: 'storeUser_store.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['storeUser'],
      template: 'src/pug/storeUser_store_edit.pug',
      inject:   'body',
      filename: 'storeUser_store_edit.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['storeUser'],
      template: 'src/pug/storeUser_store_coupon.pug',
      inject:   'body',
      filename: 'storeUser_store_coupon.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['storeUser'],
      template: 'src/pug/storeUser_store_coupon_id.pug',
      inject:   'body',
      filename: 'storeUser_store_coupon_id.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['storeUser'],
      template: 'src/pug/storeUser_store_coupon_id_edit.pug',
      inject:   'body',
      filename: 'storeUser_store_coupon_id_edit.html',
    }),

    // Compile admin-user related pages
    new HtmlWebpackPlugin({
      chunks: ['adminUser'],
      template: 'src/pug/adminUser_admin_store_create.pug',
      inject:   'body',
      filename: 'adminUser_admin_store_create.html',
    }),

    // Inject variables to JS file.
    new webpack.DefinePlugin({
      'process.env': ENV,
    }),
  ],

  postcss: () => [
    require('stylelint'),
    autoprefixer({ browsers: ['last 2 versions'] }),
    require('postcss-flexbugs-fixes'),
    require('postcss-reporter')({ clearMessages: true }),
  ],

}

// Additional webpack settings for local env (when invoked by 'npm start')
if (TARGET_ENV === 'development') {
  console.log('Serving locally...');

  module.exports = merge(commonConfig, {

    entry: {
      chat: [
        'webpack-dev-server/client?http://localhost:8080',
        path.join( __dirname, 'src/chat.js' )
      ],
      endUser: [
        'webpack-dev-server/client?http://localhost:8080',
        path.join( __dirname, 'src/endUser.js' )
      ],
      storeUser: [
        'webpack-dev-server/client?http://localhost:8080',
        path.join( __dirname, 'src/storeUser.js' )
      ],
      adminUser: [
        'webpack-dev-server/client?http://localhost:8080',
        path.join( __dirname, 'src/adminUser.js' )
      ],
    },

    devServer: {
      contentBase: 'src',
      inline:   true,
      progress: true,
    },

    module: {
      loaders: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader:  'elm-hot!elm-webpack?verbose=true&warn=true',
        },
        {
          test: /\.(css|scss)$/,
          loaders: [
            'style-loader',
            'css-loader',
            'sass-loader',
            'postcss-loader',
          ]
        }
      ]
    }
  });
}

// Additional webpack settings for prod env (when invoked via 'npm run build')
if (TARGET_ENV === 'production') {
  console.log('Building for prod...');

  module.exports = merge(commonConfig, {

    entry: {
      chat: [
        path.join( __dirname, 'src/chat.js' )
      ],
      endUser: [
        path.join( __dirname, 'src/endUser.js' )
      ],
      storeUser: [
        path.join( __dirname, 'src/storeUser.js' )
      ],
      adminUser: [
        path.join( __dirname, 'src/adminUser.js' )
      ],
    },

    module: {
      loaders: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader:  'elm-webpack',
        },
        {
          test: /\.(css|scss)$/,
          loader: ExtractTextPlugin.extract('style-loader', [
            'css-loader',
            'sass-loader',
            'postcss-loader',
          ])
        }
      ]
    },

    plugins: [
      new CopyWebpackPlugin([
        {
          from: 'src/img/',
          to:   'img/',
        },
        {
          from: 'src/favicon.ico'
        },
      ]),

      new webpack.optimize.OccurenceOrderPlugin(),

      // Extract CSS into a separate file
      new ExtractTextPlugin( './[hash].css', { allChunks: true } ),

      // Minify & mangle JS/CSS
      new webpack.optimize.UglifyJsPlugin({
          minimize:   true,
          compressor: { warnings: false }
          // mangle:  true
      }),
    ]
  });
}
