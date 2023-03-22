#r "nuget: System.IdentityModel.Tokens.Jwt, 6.25.1"
#r "nuget: Saturn, 0.16.1"

open System
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt
open Microsoft.IdentityModel.Tokens
open Saturn

let secret = "my-top-secret-no-one-knows"
let issuer = "my-domain-issuer.com"

let generateToken email =
      let claims = [|
          Claim(JwtRegisteredClaimNames.Sub, email);
          Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) |]
      claims
      |> Auth.generateJWT (secret, SecurityAlgorithms.HmacSha256) issuer (DateTime.UtcNow.AddHours(1.0))