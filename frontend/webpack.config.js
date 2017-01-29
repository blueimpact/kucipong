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
    'googleMapApiKey': JSON.stringify(
      // Default key bellow is only available on `stage.kucipong.com`.
      process.env.GOOGLE_MAP_API_KEY || 'AIzaSyA6yBv38YNHWzaAI5S7c27JfqkSFMFC-7g'
    ),
  } :
  {
    'apiRoot': JSON.stringify(
      process.env.API_ROOT || 'http://localhost:8081/v0/'
    ),
    'googleMapApiKey': JSON.stringify(
      process.env.GOOGLE_MAP_API_KEY || ''
    ),
  };

// Common webpack config
const commonConfig = {

  // Directory to output compiled files
  output: {
    path: path.resolve(__dirname, 'dist/'),
    filename: '/static/[name]-[hash].js',
  },

  resolve: {
    modulesDirectories: ['node_modules'],
    extensions: ['', '.js', '.elm'],
    root: [
      path.resolve('src/elm'),
    ]
  },

  module: {
    noParse: /\.elm$/,
    loaders: [
      {
        test: /\.(eot|ttf|woff|woff2|svg|png|jpg)$/,
        loader: 'file-loader',
        query: { name: "/static/[name]-[hash].[ext]" }
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
      filename: 'static/chat.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['chat'],
      template: 'src/style-guide/chat.html',
      inject:   'body',
      filename: 'style-guide_chat.html',
    }),

    // Compile end-user related pages
    new HtmlWebpackPlugin({
      chunks: ['endUser'],
      template: 'src/pug/endUser_category.pug',
      inject:   'body',
      filename: 'endUser_category.html',
    }),
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
      template: 'src/pug/storeUser_login.pug',
      inject:   'body',
      filename: 'storeUser_login.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['storeUser'],
      template: 'src/pug/storeUser_store.pug',
      inject:   'body',
      filename: 'storeUser_store.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['storeUser', 'storeUser_store_edit'],
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
      template: 'src/pug/adminUser_login.pug',
      inject:   'body',
      filename: 'adminUser_login.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['adminUser'],
      template: 'src/pug/adminUser_admin_store_create.pug',
      inject:   'body',
      filename: 'adminUser_admin_store_create.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['adminUser'],
      template: 'src/pug/adminUser_admin_store_delete.pug',
      inject:   'body',
      filename: 'adminUser_admin_store_delete.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['adminUser'],
      template: 'src/pug/adminUser_admin_store_delete_confirm.pug',
      inject:   'body',
      filename: 'adminUser_admin_store_delete_confirm.html',
    }),
    new HtmlWebpackPlugin({
      chunks: ['adminUser'],
      template: 'src/pug/adminUser_admin_store_login.pug',
      inject:   'body',
      filename: 'adminUser_admin_store_login.html',
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
      "storeUser_store_edit": [
        'webpack-dev-server/client?http://localhost:8080',
        path.join( __dirname, 'src/js/storeUser_store_edit.js' )
      ],
      adminUser: [
        'webpack-dev-server/client?http://localhost:8080',
        path.join( __dirname, 'src/adminUser.js' )
      ],
      magicLogin: [
        'webpack-dev-server/client?http://localhost:8080',
        path.join( __dirname, 'src/magicLogin.js' )
      ],
    },

    devtool: 'source-map',

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
            'style',
            'css',
            'resolve-url',
            'sass',
            'postcss',
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
      "storeUser_store_edit": [
        'webpack-dev-server/client?http://localhost:8080',
        path.join( __dirname, 'src/js/storeUser_store_edit.js' )
      ],
      adminUser: [
        path.join( __dirname, 'src/adminUser.js' )
      ],
      magicLogin: [
        path.join( __dirname, 'src/magicLogin.js' )
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
          loader: ExtractTextPlugin.extract('style', [
            'css',
            'resolve-url',
            'sass',
            'postcss',
          ])
        }
      ]
    },

    plugins: [
      new CopyWebpackPlugin([
        {
          from: 'src/favicon.ico'
        },
      ]),

      new webpack.optimize.OccurenceOrderPlugin(),

      // Extract CSS into a separate file
      new ExtractTextPlugin( '/static/[name]-[hash].css', { allChunks: true } ),

      // Minify & mangle JS/CSS
      new webpack.optimize.UglifyJsPlugin({
          minimize:   true,
          compressor: { warnings: false }
          // mangle:  true
      }),
    ]
  });
}
