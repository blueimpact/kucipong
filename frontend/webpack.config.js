const path              = require('path');
const webpack           = require('webpack');
const merge             = require('webpack-merge');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

console.log('Start Webpack process...');

// Determine build env by npm command options
const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? 'production' : 'development';
const ENV = {
  'googleMapApiKey':
    process.env.GOOGLE_MAP_API_KEY ||
      // This API key is only available on `stage.kucipong.com`.
      'AIzaSyA6yBv38YNHWzaAI5S7c27JfqkSFMFC-7g',
};

// Common webpack config
const commonConfig = {

  output: {
    path: path.resolve(__dirname, 'dist/'),
    filename: '/static/[name].js',
  },

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
    "storeUser_store_edit": [
      path.join( __dirname, 'src/js/storeUser_store_edit.js' )
    ],
    "storeUser_store_coupon_id_edit": [
      path.join( __dirname, 'src/js/storeUser_store_coupon_id_edit.js' )
    ],
  },

  resolve: {
    extensions: ['.js', '.elm'],
    modules: [
      'node_modules',
      path.resolve('src/elm'),
    ],
  },

  module: {
    rules: [
      {
        test: /\.(eot|ttf|woff|woff2|svg)$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              name: "/static/[name]-[hash].[ext]",
            },
          },
        ]
      },
      {
        test: /\.pug$/,
        use: 'pug-loader',
      },
      {
        test: /\.(jpg|jpeg|png)$/,
        use: 'url-loader'
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
      data: ENV,
    }),
    new HtmlWebpackPlugin({
      chunks: ['endUser'],
      template: 'src/pug/endUser_store_id.pug',
      inject:   'body',
      filename: 'endUser_store_id.html',
      data: ENV,
    }),
    new HtmlWebpackPlugin({
      chunks: ['endUser'],
      template: 'src/pug/endUser_store_id_coupon.pug',
      inject:   'body',
      filename: 'endUser_store_id_coupon.html',
      data: ENV,
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
      data: ENV,
    }),
    new HtmlWebpackPlugin({
      chunks: ['storeUser', 'storeUser_store_edit'],
      template: 'src/pug/storeUser_store_edit.pug',
      inject:   'body',
      filename: 'storeUser_store_edit.html',
      data: ENV,
    }),
    new HtmlWebpackPlugin({
      chunks: ['storeUser'],
      template: 'src/pug/storeUser_store_coupon.pug',
      inject:   'body',
      filename: 'storeUser_store_coupon.html',
      data: ENV,
    }),
    new HtmlWebpackPlugin({
      chunks: ['storeUser'],
      template: 'src/pug/storeUser_store_coupon_id.pug',
      inject:   'body',
      filename: 'storeUser_store_coupon_id.html',
      data: ENV,
    }),
    new HtmlWebpackPlugin({
      chunks: ['storeUser', 'storeUser_store_coupon_id_edit'],
      template: 'src/pug/storeUser_store_coupon_id_edit.pug',
      inject:   'body',
      filename: 'storeUser_store_coupon_id_edit.html',
      data: ENV,
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
      'process.env':
        Object.keys(ENV).reduce((o, k) =>
          merge(o, {
            [k]: JSON.stringify(ENV[k]),
          }), {}
        ),
    }),
  ],
};

// Settings for `npm start`
if (TARGET_ENV === 'development') {
  console.log('Serving locally...');

  module.exports = merge(commonConfig, {

    devtool: 'source-map',
    devServer: {
      contentBase: 'src',
      inline: true,
      port: ENV.port,
      host: ENV.host,
    },

    module: {
      rules: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            {
              loader: 'elm-hot-loader',
            },
            { loader: 'elm-webpack-loader',
              options: {
                verbose: true,
                warn: true,
              },
            },
          ],
        },
        {
          test: /\.(css|scss)$/,
          use: [
            'style-loader',
            'css-loader',
            'resolve-url-loader',
            'sass-loader',
            'postcss-loader',
          ],
        },
      ],
    },
  });
}

// Settings for `npm run build`.
if (TARGET_ENV === 'production') {
  console.log('Building for prod...');

  module.exports = merge(commonConfig, {

    module: {
      rules: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader:  'elm-webpack-loader',
        },
        {
          test: /\.(css|scss)$/,
          use: ExtractTextPlugin.extract({
            fallback: 'style-loader',
            use: [
              'css-loader',
              'resolve-url-loader',
              'sass-loader',
              'postcss-loader',
            ],
          }),
        },
      ],
    },

    plugins: [
      new CopyWebpackPlugin([
        {
          from: 'src/favicon.ico'
        },
      ]),

      new webpack.optimize.OccurenceOrderPlugin(),

      // Extract CSS into a separate file
      new ExtractTextPlugin( '/static/[name].css', { allChunks: true } ),

      // Minify & mangle JS/CSS
      new webpack.optimize.UglifyJsPlugin({
          minimize:   true,
          compressor: { warnings: false }
      }),
    ],
  });
}
