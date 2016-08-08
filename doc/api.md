# Back-end API specification

## End user API

For the user who check the coupon.

* No authentication is required for any API
* No user specific information is saved on back-end side
    * Any such data are saved on front-end with localStorage.

### GET recommended coupons

GET recommended coupons data for current user settings.

#### Sample request and response

Though the behavior of this API implies that it's better to use GET method, it might be good to use POST method to send this complex request with full JSON expression.

```bash
$ curl -G "http://$domain/api/v1/coupon" \
  --data-urlencode 'areas=[{"latitude": 41.40338, "longitude": 2.17403}]' \
  --data-urlencode 'tags=[4,6,8,9]' \
  --data-urlencode 'category=1' \
  --data-urlencode 'favorites=[2,50,230]' \
  --data-urlencode 'history=[430,2,220,50,22,300,230]' \
  | jq '.'

{
  "next": null,
  "coupons": [
    {
      "id": 3,
      "store": {
        "id": 52,
        "category": 1,
        "tags": [3, 4, 8],
        "image": "http://s3.amasonaws.com/foo/baz",
        "description": "七輪焼き肉・安安は、美味しい焼肉を...",
        "address": "渋谷区桜丘町2-12 渋谷亀八ビル4F",
        "phoneNumber": "03-3464-0722",
        "hours": "月〜木 17:00〜翌4:30\n金土日祝・祝前 16:00〜翌4:30",
        "closeOn": "元旦のみ",
        "url": "http://www.fuji-tatsu.co.jp/",
      },
      "image": "http://s3.amasonaws.com/foo/bar",
      "title": "当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円",
      "validFrom": "Sun, 15 May 2016 00:00:00 GMT",
      "expire": "Mon, 30 May 2016 00:00:00 GMT",
      "type": "set",
      "setContent": "2.5時間飲放題付きの料理4品です。\n1. 前菜\n2. 焼き肉盛り合わせ\n 3. 冷麺\n 4. アイスクリーム",
      "setPrice": 3600,
      "setReferencePrice": 4000,
      "setOtherConditions": "※ 1グループ1回のご利用時の利用枚数制限はありません。\n※ 21時以降のご予約のお客様が対象です。"
    },
    ...
  ]
}
```

#### Request Parameter and its type

```haskell
-- Dummy data type representing request parameter from front-end
data RequestParam = RequestParam
  {
    -- Only coupons whose location is near by the any area
    -- specified by this parameter
    areas :: [Area]
    -- Tag IDs that the user chose as he/she is interested in
  , tags  :: [Tag]
    -- Category ID of coupons to show
  , category :: Category
    -- Coupon IDs that the user put it into favorite box
    -- The left side is the coupon ID that the user put it into for the last time
  , favorites :: [CouponId]
    -- Coupon IDs that the user has ever seen details
    -- For practical, the front-end will cut off only N newest history
    -- The left side is the coupon ID that the user saw for the last time
  , history :: [CouponId]
    -- This parameter is for infinite loading
    -- The response generate resulting list of coupons, and drops while this Coupon appears
    -- i.e., The first coupon id of response is this value
    -- If this value is `Nothing`, DO NOT drop coupons
  , next :: Maybe CouponId
    -- Response only returns first N of resulting lists,
    -- where N is the value specified by this
    -- If not specified, first 100 (default value) coupons are returned
  , limit :: Maybe Int
  }

-- ==============
--  Helper Types
-- ==============

data Area = Area
  { latitude :: Latitude
  , longitude :: Longitude
  }
newtype Latitude = Latitude { unLatitude :: Double }
newtype Longitude = Longitude { unLongitude :: Double }
newtype Tag = Tag { unTag :: Int }
newtype Category = Category { unCategory :: Int }
newtype CouponId = CouponId { unCouponId :: Int }
```

#### Response and its type

This code is for explanation of the API response, so this is NOT the same structure as production code.

```haskell
-- Dummy data type representing response from back-end
data Response = Response
  {
    -- Next cursor of this search parameters
    -- i.e., To implement infinite scroll,
    --       the front-end send next request
    --       by setting this value on `next` parameter of request
    next :: Maybe CouponId
    -- Coupons matching request search parameters
  , coupons :: [Coupon]
  }

data Coupon = Coupon
  {
    -- An ID of this coupon
    couponId :: CouponId
    -- Information about the store that provide this coupon
  , couponStore :: Store
    -- An image for illustration purposes of the coupon
    -- If the image does not present, this returns default image
  , couponImage :: Image
    -- A title of this coupon
  , couponTitle :: Maybe Text
    -- The coupon can be used from this date
    -- Also, the coupon cannot be seen by end-user before this date
  , couponValidFrom :: Maybe UTCDate
    -- Expire date of this coupon
    -- After this date, the coupon does not appear in kucipong search result
    -- The store can reject the coupon, if an end-user show this coupon after the expire date
  , couponExpire :: Maybe UTCDate
    -- Coupon type
  , couponType :: CouponType

    -- ======== For `couponType` is `CouponDiscount` ========
    -- Percentage style discount rate about total fee
    -- i.e., (Actual fee) = (Total fee) * (100 - discountRate) `div` 100
  , discountRate :: Maybe PercentInt
    -- Minimum total fee to use this coupon
  , discountMinimumFee :: Maybe Int
    -- Other conditions to use this coupon
  , discountOtherConditions :: Maybe Text

    -- ======== For `couponType` is `CouponGift` ========
    -- Description about the gift
  , giftContent :: Maybe Text
    -- Reference price of the gift
  , giftReferencePrice :: Maybe Int
    -- Minimum total fee to use this coupon
  , giftMinimumFee :: Maybe Int
    -- Other conditions to use this coupon
  , giftOtherConditions :: Maybe Text

    -- ======== For `couponType` is `CouponSet` ========
    -- Description about the  set
  , setContent :: Maybe Text
    -- The  price of the set
  , setPrice :: Maybe Int
    -- Reference price of the  set
    -- i.e., The total fee to buy the same items without this coupon
  , setReferencePrice :: Maybe Int
    -- Other conditions to use this coupon
  , setOtherConditions :: Maybe Text

    -- ======== For `couponType` is `CouponOther` ========
    -- Desctiption about this coupon
  , otherContent :: Maybe Text
    -- Conditions to use this coupon
  , otherConditions :: Maybe Text
  }

data Store = Store
  {
    -- An ID of this store
    storeId :: StoreId
    -- Store name
  , storeName :: Maybe Text
    -- Store category
  , storeCategory :: Maybe Category
    -- Tags symbolizing the store
  , storeTags :: [Tag]
    -- Main image illustrating the store
  , storeImage :: Maybe Image
    -- Store description to show end-users
  , storeDescription :: Maybe Text
    -- Store address
  , storeAddress :: Maybe Text
    -- Phone number end-users can contact store staffs
  , storePhoneNumber :: Maybe Text
    -- What time is the store open?
  , storeHours :: Maybe Text
    -- What day is it the store closed?
  , storeCloseOn :: Maybe Text
    -- Official URL of the store
  , storeURL :: Maybe URL
  }

data CouponType =
    -- A coupon to discount total amount of fee
    CouponDiscount
    -- A coupon to get free gifts if buy something
  | CouponGift
    -- A coupon to buy special set with special price
  | CouponSet
    -- A coupon of other format
  | CouponOther

-- ==============
--  Helper Types
-- ==============

newtype Image = Image { unImage :: Text }
newtype PercentInt = PercentInt { unPercentInt :: Int }
```
