import { Injectable } from '@angular/core';
import { MessageService } from './message.service';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { catchError, Observable, of } from 'rxjs';
import { Router } from '@angular/router';

interface UserLogin {
  email: string;
  password : string;
}

interface LoginResponse {
  token: string;
}

interface DecodeResponse {
  email: string;
  roles: string[];
}

interface UserStaff {
  email: string;
  password: string;
  phone: string;
  role: string;
}

interface CreateStaffUserResponse {
  message: string;
}

interface UserInfo {
  email: string;
  roles: string[];
  token: string;
}

@Injectable({
  providedIn: 'root'
})

export class UserService {
  theServerURL = 'https://localhost:5001/api';
  httpOptions = {
    headers: new HttpHeaders({'Content-Type': 'application/json' })
  };
  
  constructor(private messageService: MessageService, private http: HttpClient, private router : Router) { }

  public login(userEmail : string, userPassword : string) {
    const url = `${this.theServerURL}/login-internal`;
    let user: UserLogin = {
      email : userEmail,
      password : userPassword
    };

    this.http.post<LoginResponse>(url, user, this.httpOptions) 
    .pipe(catchError(this.handleError<LoginResponse>('User login'))) 
    .subscribe(data => {
      if (data && data.token) { 
        console.log(data.token);
        this.decodeTokenandRedirect(data.token); 
        this.log(`User was successfully logged in.`);
      } else {
        this.log(`Login failed: No token received.`);
      }
    });

  }

  public createStaffUser(userEmail : string, userPassword : string, userPhone : string, userRole : string) {
    const url = `${this.theServerURL}/create-staff`;
    let user: UserStaff = {
      email : userEmail,
      password : userPassword,
      phone : userPhone,
      role : userRole
    };

    this.http.post<CreateStaffUserResponse>(url, user, this.httpOptions) 
    .pipe(catchError(this.handleError<CreateStaffUserResponse>('Create staff user'))) 
    .subscribe(data => {
      if (data.message) { 
        console.log(data.message);
        this.log(data.message);
      } else {
        this.log(`Creation of user failed.`);
      }
    });

  }


  private decodeTokenandRedirect(token: string) {
    const url = `${this.theServerURL}/decode-token?token=${encodeURIComponent(token)}`;
  
    this.http.get<DecodeResponse>(url, this.httpOptions) // Send an empty body
      .pipe(catchError(this.handleError<DecodeResponse>('Role identification')))
      .subscribe(data => {
        console.log(data);
        if (data && data.roles && data.roles.length > 0) {
          const roles = data.roles;

          const userInfo : UserInfo ={
            email : data.email,
            roles : data.roles,
            token : token
          }

          localStorage.setItem('user',JSON.stringify(userInfo))

          if (roles.includes('Admin')) {
            this.router.navigate(['/admin']);
          } else if (roles.includes('Doctor')) {
            this.router.navigate(['/doctor']);
          } else if (roles.includes('Nurse')) {
            this.router.navigate(['/staff']);
          } else if (roles.includes('Patient')) {
            this.router.navigate(['/patient']);
          }

          } else {
          console.error('No roles found to be associated with the user.');
        }
      });
  }
  

  //------------------------/------------------------/------------------------
  private handleError<T>(operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {

      if (error.status === 440) {
        this.log("Error: Login session expired.");
        return of(result as T);
      } else if (error.status === 401) {
        this.log("Error: Authentication is required.");
        return of(result as T);
      } else if (error.status === 403) {
        this.log("Error: Not allowed to access the feature.");
        return of(result as T);
      } else if(error.status === 200 ||error.status === 400 ){
        this.log(`${operation} failed: ${error.error}`);
      } else {
        this.log(`${operation} failed: an unexpected error occured.`);
      }
      return of(result as T);
    };
  }

  private log(message: string, p0?: { data: UserLogin; }) {
    this.messageService.add(`${message}`);
  }
}
